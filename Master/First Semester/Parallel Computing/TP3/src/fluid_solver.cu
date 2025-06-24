#include "fluid_solver.h"
#include <cmath>
#include <omp.h>
#include <cuda_runtime.h>
#include <device_launch_parameters.h>
#include <iostream>

#define IX(i, j, k) ((i) + (M + 2) * (j) + (M + 2) * (N + 2) * (k))
#define SWAP(x0, x)      \
    {                    \
        float *tmp = x0; \
        x0 = x;          \
        x = tmp;         \
    }
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#define LINEARSOLVERTIMES 20
#define BLOCK 8
#define BLOCK_SIZE BLOCK *BLOCK *BLOCK

__global__ void addSourceKernel(int M, int N, int O, float *x, float *s, float dt)
{

    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    int idy = blockIdx.y * blockDim.y + threadIdx.y;
    int idz = blockIdx.z * blockDim.z + threadIdx.z;

    if (idx < M + 2 && idy < N + 2 && idz < O + 2)
    {
        int index = IX(idx, idy, idz);
        x[index] += dt * s[index];
    }
}

void add_source(int M, int N, int O, float *x, float *s, float dt)
{
    dim3 block(BLOCK, BLOCK, BLOCK);
    dim3 grid((M + block.x - 1) / block.x,
              (N + block.y - 1) / block.y,
              (O + block.z - 1) / block.z);

    addSourceKernel<<<grid, block>>>(M, N, O, x, s, dt);
}

__global__ void set_bnd_kernel(int M, int N, int O, int b, float *x)
{
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    if (idx <= M * N)
    {
        int i = idx % M + 1;
        int j = idx / M + 1;
        if (i <= M && j <= N)
        {
            x[IX(i, j, 0)] = b == 3 ? -x[IX(i, j, 1)] : x[IX(i, j, 1)];
            x[IX(i, j, O + 1)] = b == 3 ? -x[IX(i, j, O)] : x[IX(i, j, O)];
        }
    }

    if (idx <= M * O)
    {
        int i = idx % M + 1;
        int k = idx / M + 1;
        if (i <= M && k <= O)
        {
            x[IX(i, 0, k)] = b == 2 ? -x[IX(i, 1, k)] : x[IX(i, 1, k)];
            x[IX(i, N + 1, k)] = b == 2 ? -x[IX(i, N, k)] : x[IX(i, N, k)];
        }
    }

    if (idx <= N * O)
    {
        int j = idx % N + 1;
        int k = idx / N + 1;
        if (j <= N && k <= O)
        {
            x[IX(0, j, k)] = b == 1 ? -x[IX(1, j, k)] : x[IX(1, j, k)];
            x[IX(M + 1, j, k)] = b == 1 ? -x[IX(M, j, k)] : x[IX(M, j, k)];
        }
    }

    if (idx == 0) // Garantir que só uma thread faz isso
    {
        x[IX(0, 0, 0)] = 0.33f * (x[IX(1, 0, 0)] + x[IX(0, 1, 0)] + x[IX(0, 0, 1)]);
        x[IX(M + 1, 0, 0)] = 0.33f * (x[IX(M, 0, 0)] + x[IX(M + 1, 1, 0)] + x[IX(M + 1, 0, 1)]);
        x[IX(0, N + 1, 0)] = 0.33f * (x[IX(1, N + 1, 0)] + x[IX(0, N, 0)] + x[IX(0, N + 1, 1)]);
        x[IX(M + 1, N + 1, 0)] = 0.33f * (x[IX(M, N + 1, 0)] + x[IX(M + 1, N, 0)] + x[IX(M + 1, N + 1, 1)]);
        x[IX(0, 0, O + 1)] = 0.33f * (x[IX(1, 0, O + 1)] + x[IX(0, 1, O + 1)] + x[IX(0, 0, O)]);
        x[IX(M + 1, 0, O + 1)] = 0.33f * (x[IX(M, 0, O + 1)] + x[IX(M + 1, 1, O + 1)] + x[IX(M + 1, 0, O)]);
        x[IX(0, N + 1, O + 1)] = 0.33f * (x[IX(1, N + 1, O + 1)] + x[IX(0, N, O + 1)] + x[IX(0, N + 1, O)]);
        x[IX(M + 1, N + 1, O + 1)] = 0.33f * (x[IX(M, N + 1, O + 1)] + x[IX(M + 1, N, O + 1)] + x[IX(M + 1, N + 1, O)]);
    }
}

void set_bnd(int M, int N, int O, int b, float *x)
{
    int maxSize = MAX(MAX(M * N, M * O), N * O);
    int block = BLOCK_SIZE;
    int numBlocks = (maxSize + block - 1) / block;
    set_bnd_kernel<<<numBlocks, block>>>(M, N, O, b, x);
}

__global__ void lin_solve_kernel(int M, int N, int O, float *x, float *x0,
                                float a, float c, int phase, float *max_change)
{
    int tx = threadIdx.x;
    int ty = threadIdx.y;
    int tz = threadIdx.z;
    int j = blockIdx.y * (blockDim.y - 2) + ty;
    int k = blockIdx.z * (blockDim.z - 2) + tz;
    int i = 2 * (blockIdx.x * (blockDim.x - 2) + tx) + ((j + k) % 2 == phase ? 0 : 1); 
    if (tx > 0 && tx < blockDim.x -1 &&
        ty > 0 && ty < blockDim.y -1 &&
        tz > 0 && tz < blockDim.z -1 &&
        i <= M && j <= N && k <= O)
    {   
        float old_x = x[IX(i, j, k)];
        float new_x = (x0[IX(i, j, k)] +
                       a * (x[IX(i-1, j, k)] + x[IX(i+1, j, k)] +
                            x[IX(i, j-1, k)] + x[IX(i, j+1, k)] +
                            x[IX(i, j, k-1)] + x[IX(i, j, k+1)])) / c;

        x[IX(i, j, k)] = new_x;
        float change = fabsf(new_x - old_x);
        if (change > *max_change)
        {
            atomicMax((int *)max_change, __float_as_int(change));
        }
    }
}
void lin_solve(int M, int N, int O, int b, float *x, float *x0, float a, float c)
{
    dim3 block(BLOCK - 1, BLOCK - 1, BLOCK - 1);
    dim3 grid(((M/2) + block.x - 1) / block.x,
              (N + block.y - 1) / block.y,
              (O + block.z - 1) / block.z);
    float *max_change_dev;
    cudaMalloc(&max_change_dev, sizeof(float));
    float max_change_host;
    int iter = 0;
    float tol = 1e-7f;
    do
    {
        float init_max = 0.0f;
        cudaMemcpy(max_change_dev, &init_max, sizeof(float), cudaMemcpyHostToDevice);
        lin_solve_kernel<<<grid, block>>>(M, N, O, x, x0, a, c, 0, max_change_dev);
        lin_solve_kernel<<<grid, block>>>(M, N, O, x, x0, a, c, 1, max_change_dev);
        set_bnd_kernel<<<(MAX(MAX(M, N), O) + 255) / 256, 256>>>(M, N, O, b, x);
        cudaMemcpy(&max_change_host, max_change_dev, sizeof(float), cudaMemcpyDeviceToHost);
        iter++;
    } while (max_change_host > tol && iter < LINEARSOLVERTIMES);
    cudaFree(max_change_dev);
}

void diffuse(int M, int N, int O, int b, float *x, float *x0, float diff,
             float dt)
{
    int max = MAX(MAX(M, N), O);
    float a = dt * diff * max * max;
    lin_solve(M, N, O, b, x, x0, a, 1 + 6 * a);
}

__global__ void advectKernel(int M, int N, int O, int b, float *d, float *d0,
                             float *u, float *v, float *w, float dt)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x + 1;
    int j = blockIdx.y * blockDim.y + threadIdx.y + 1;
    int k = blockIdx.z * blockDim.z + threadIdx.z + 1;

    if (i <= M && j <= N && k <= O)
    {
        float dtX = dt * M;
        float dtY = dt * N;
        float dtZ = dt * O;

        float x = i - dtX * u[IX(i, j, k)];
        float y = j - dtY * v[IX(i, j, k)];
        float z = k - dtZ * w[IX(i, j, k)];

        x = fminf(fmaxf(x, 0.5f), M + 0.5f);
        y = fminf(fmaxf(y, 0.5f), N + 0.5f);
        z = fminf(fmaxf(z, 0.5f), O + 0.5f);

        int i0 = (int)x;
        int i1 = i0 + 1;
        int j0 = (int)y;
        int j1 = j0 + 1;
        int k0 = (int)z;
        int k1 = k0 + 1;

        float s1 = x - i0;
        float s0 = 1.0f - s1;
        float t1 = y - j0;
        float t0 = 1.0f - t1;
        float u1 = z - k0;
        float u0 = 1.0f - u1;

        d[IX(i, j, k)] =
            s0 * (t0 * (u0 * d0[IX(i0, j0, k0)] + u1 * d0[IX(i0, j0, k1)]) +
                  t1 * (u0 * d0[IX(i0, j1, k0)] + u1 * d0[IX(i0, j1, k1)])) +
            s1 * (t0 * (u0 * d0[IX(i1, j0, k0)] + u1 * d0[IX(i1, j0, k1)]) +
                  t1 * (u0 * d0[IX(i1, j1, k0)] + u1 * d0[IX(i1, j1, k1)]));
    }
}

void advect(int M, int N, int O, int b, float *d, float *d0,
            float *u, float *v, float *w, float dt)
{
    dim3 block(BLOCK, BLOCK, BLOCK);
    dim3 grid(
        (M + block.x - 1) / block.x,
        (N + block.y - 1) / block.y,
        (O + block.z - 1) / block.z);

    advectKernel<<<grid, block>>>(M, N, O, b, d, d0, u, v, w, dt);

    set_bnd(M, N, O, b, d);
}

__global__ void project_kernel_1(int M, int N, int O, float *u, float *v,
                                 float *w, float *p, float *div, float max1)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x + 1;
    int j = blockIdx.y * blockDim.y + threadIdx.y + 1;
    int k = blockIdx.z * blockDim.z + threadIdx.z + 1;

    if (i <= M && j <= N && k <= O)
    {
        div[IX(i, j, k)] = -0.5f *
                           (u[IX(i + 1, j, k)] - u[IX(i - 1, j, k)] +
                            v[IX(i, j + 1, k)] - v[IX(i, j - 1, k)] +
                            w[IX(i, j, k + 1)] - w[IX(i, j, k - 1)]) /
                           max1;

        p[IX(i, j, k)] = 0.0f;
    }
}

__global__ void project_kernel_2(int M, int N, int O, float *u, float *v,
                                 float *w, float *p)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x + 1;
    int j = blockIdx.y * blockDim.y + threadIdx.y + 1;
    int k = blockIdx.z * blockDim.z + threadIdx.z + 1;

    if (i <= M && j <= N && k <= O)
    {
        u[IX(i, j, k)] -= 0.5f * (p[IX(i + 1, j, k)] - p[IX(i - 1, j, k)]);
        v[IX(i, j, k)] -= 0.5f * (p[IX(i, j + 1, k)] - p[IX(i, j - 1, k)]);
        w[IX(i, j, k)] -= 0.5f * (p[IX(i, j, k + 1)] - p[IX(i, j, k - 1)]);
    }
}

void project(int M, int N, int O, float *u, float *v, float *w, float *p, float *div)
{
    float max1 = (float)MAX(M, MAX(N, O));

    dim3 block(BLOCK, BLOCK, BLOCK);
    dim3 grid(
        (M + block.x - 1) / block.x,
        (N + block.y - 1) / block.y,
        (O + block.z - 1) / block.z);

    project_kernel_1<<<grid, block>>>(M, N, O, u, v, w, p, div, max1);

    set_bnd(M, N, O, 0, div);
    set_bnd(M, N, O, 0, p);

    lin_solve(M, N, O, 0, p, div, 1, 6);

    project_kernel_2<<<grid, block>>>(M, N, O, u, v, w, p);

    set_bnd(M, N, O, 1, u);
    set_bnd(M, N, O, 2, v);
    set_bnd(M, N, O, 3, w);
}


void dens_step(int M, int N, int O, float *x, float *x0, float *u, float *v,
               float *w, float diff, float dt)
{
    add_source(M, N, O, x, x0, dt);
    // SWAP(x0, x);
    diffuse(M, N, O, 0, x0, x, diff, dt);
    // SWAP(x0, x);
    advect(M, N, O, 0, x, x0, u, v, w, dt);
}

void vel_step(int M, int N, int O, float *u, float *v, float *w, float *u0,
              float *v0, float *w0, float visc, float dt)
{
    add_source(M, N, O, u, u0, dt);
    add_source(M, N, O, v, v0, dt);
    add_source(M, N, O, w, w0, dt);
    // SWAP(u0, u);
    diffuse(M, N, O, 1, u0, u, visc, dt); // SWAP(u0, u);
    // SWAP(v0, v);
    diffuse(M, N, O, 2, v0, v, visc, dt); // SWAP(v0, v);
    // SWAP(w0, w);
    diffuse(M, N, O, 3, w0, w, visc, dt); // SWAP(w0, w);
    project(M, N, O, u0, v0, w0, u, v);   // 3 swaps ao mesmo tempo

    // SWAP(u0, u);
    // SWAP(v0, v);
    // SWAP(w0, w);

    // Aqui voltamos aos originais pois 2 swaps é o mesmo que nada
    advect(M, N, O, 1, u, u0, u0, v0, w0, dt);
    advect(M, N, O, 2, v, v0, u0, v0, w0, dt);
    advect(M, N, O, 3, w, w0, u0, v0, w0, dt);
    project(M, N, O, u, v, w, u0, v0);
}
