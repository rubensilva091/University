#include "fluid_solver.h"
#include <cmath>
#include <omp.h>
#include <iostream>

#define IX(i, j, k) ((i) + (M + 2) * (j) + (M + 2) * (N + 2) * (k))
#define SWAP(x0, x)  \
  {                  \
    float *tmp = x0; \
    x0 = x;          \
    x = tmp;         \
  }
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#define LINEARSOLVERTIMES 20

// Add sources (density or velocity)
void add_source(int M, int N, int O, float *x, float *s, float dt)
{
  int size = (M + 2) * (N + 2) * (O + 2);
  for (int i = 0; i < size; i++)
  {
    x[i] += dt * s[i];
  }
}

// Set boundary conditions
void set_bnd(int M, int N, int O, int b, float *x)
{
  int i, j;

// Set boundary on faces
#pragma omp parallel
  {
#pragma omp for schedule(dynamic)
    for (j = 1; j <= N; j++)
    {
      for (i = 1; i <= M; i++)
      {
        x[IX(i, j, 0)] = b == 3 ? -x[IX(i, j, 1)] : x[IX(i, j, 1)];
        x[IX(i, j, O + 1)] = b == 3 ? -x[IX(i, j, O)] : x[IX(i, j, O)];
      }
    }
#pragma omp for schedule(dynamic)
    for (j = 1; j <= O; j++)
    {
      for (i = 1; i <= N; i++)
      {
        x[IX(0, i, j)] = b == 1 ? -x[IX(1, i, j)] : x[IX(1, i, j)];
        x[IX(M + 1, i, j)] = b == 1 ? -x[IX(M, i, j)] : x[IX(M, i, j)];
      }
    }
#pragma omp for schedule(dynamic)
    for (j = 1; j <= O; j++)

    {
      for (i = 1; i <= M; i++)
      {
        x[IX(i, 0, j)] = b == 2 ? -x[IX(i, 1, j)] : x[IX(i, 1, j)];
        x[IX(i, N + 1, j)] = b == 2 ? -x[IX(i, N, j)] : x[IX(i, N, j)];
      }
    }
  }

  // Set corners
  x[IX(0, 0, 0)] = 0.33f * (x[IX(1, 0, 0)] + x[IX(0, 1, 0)] + x[IX(0, 0, 1)]);
  x[IX(M + 1, 0, 0)] =
      0.33f * (x[IX(M, 0, 0)] + x[IX(M + 1, 1, 0)] + x[IX(M + 1, 0, 1)]);
  x[IX(0, N + 1, 0)] =
      0.33f * (x[IX(1, N + 1, 0)] + x[IX(0, N, 0)] + x[IX(0, N + 1, 1)]);
  x[IX(M + 1, N + 1, 0)] = 0.33f * (x[IX(M, N + 1, 0)] + x[IX(M + 1, N, 0)] +
                                    x[IX(M + 1, N + 1, 1)]);
}

void lin_solve(int M, int N, int O, int b, float *x, float *x0, float a, float c)
{
  float tol = 1e-7, max_c, old_x, change;
  int l = 0;
  float new_c = 1 / c;
  do
  {
    max_c = 0.0f;
#pragma omp parallel reduction(max : max_c)
    {
#pragma omp for schedule(static) private(old_x, change)
      for (int k = 1; k <= O; k++)
      {
        for (int j = 1; j <= N; j++)
        {
          for (int i = 1 + (k + j) % 2; i <= M; i += 2)
          {
            old_x = x[IX(i, j, k)];
            x[IX(i, j, k)] = (x0[IX(i, j, k)] +
                              a * (x[IX(i - 1, j, k)] + x[IX(i + 1, j, k)] +
                                   x[IX(i, j - 1, k)] + x[IX(i, j + 1, k)] +
                                   x[IX(i, j, k - 1)] + x[IX(i, j, k + 1)])) *
                             new_c;
            change = fabs(x[IX(i, j, k)] - old_x);
            if (change > max_c)
              max_c = change;
          }
        }
      }
#pragma omp for schedule(static) private(old_x, change)
      for (int k = 1; k <= O; k++)
      {
        for (int j = 1; j <= N; j++)
        {
          for (int i = 1 + (k + j + 1) % 2; i <= M; i += 2)
          {
            old_x = x[IX(i, j, k)];
            x[IX(i, j, k)] = (x0[IX(i, j, k)] +
                              a * (x[IX(i - 1, j, k)] + x[IX(i + 1, j, k)] +
                                   x[IX(i, j - 1, k)] + x[IX(i, j + 1, k)] +
                                   x[IX(i, j, k - 1)] + x[IX(i, j, k + 1)])) *
                             new_c;
            change = fabs(x[IX(i, j, k)] - old_x);
            if (change > max_c)
              max_c = change;
          }
        }
      }
    } 
    set_bnd(M, N, O, b, x);
  } while (max_c > tol && ++l < 20);
}

// Diffusion step (uses implicit method)
void diffuse(int M, int N, int O, int b, float *x, float *x0, float diff,
             float dt)
{
  int max = MAX(MAX(M, N), O);
  float a = dt * diff * max * max;
  lin_solve(M, N, O, b, x, x0, a, 1 + 6 * a);
}

// Advection step (uses velocity field to move quantities) OPTIMIZED !!!
void advect(int M, int N, int O, int b, float *d, float *d0, float *u, float *v, float *w, float dt)
{
  float dtX = dt * M, dtY = dt * N, dtZ = dt * O;
  int tile_size = 20;

#pragma omp parallel for collapse(2) schedule(dynamic)
  for (int jj = 1; jj <= N; jj += tile_size)
  {
    for (int kk = 1; kk <= O; kk += tile_size)
    {
      for (int ii = 1; ii <= M; ii += tile_size)
      {
        int block_Limit_J = (jj + tile_size < N + 1) ? jj + tile_size : N + 1;
        int block_Limit_K = (kk + tile_size < O + 1) ? kk + tile_size : O + 1;
        int block_Limit_I = (ii + tile_size < M + 1) ? ii + tile_size : M + 1;

        for (int j = jj; j < block_Limit_J; j++)
        {
          for (int k = kk; k < block_Limit_K; k++)
          {
            for (int i = ii; i < block_Limit_I; i++)
            {
              float x = i - dtX * u[IX(i, j, k)];
              float y = j - dtY * v[IX(i, j, k)];
              float z = k - dtZ * w[IX(i, j, k)];

              // Clamp to grid boundaries
              x = fminf(fmaxf(x, 0.5f), M + 0.5f);
              y = fminf(fmaxf(y, 0.5f), N + 0.5f);
              z = fminf(fmaxf(z, 0.5f), O + 0.5f);

              int i0 = (int)x, i1 = i0 + 1;
              int j0 = (int)y, j1 = j0 + 1;
              int k0 = (int)z, k1 = k0 + 1;

              float s1 = x - i0, s0 = 1 - s1;
              float t1 = y - j0, t0 = 1 - t1;
              float u1 = z - k0, u0 = 1 - u1;

              d[IX(i, j, k)] = s0 * (t0 * (u0 * d0[IX(i0, j0, k0)] + u1 * d0[IX(i0, j0, k1)]) +
                                     t1 * (u0 * d0[IX(i0, j1, k0)] + u1 * d0[IX(i0, j1, k1)])) +
                               s1 * (t0 * (u0 * d0[IX(i1, j0, k0)] + u1 * d0[IX(i1, j0, k1)]) +
                                     t1 * (u0 * d0[IX(i1, j1, k0)] + u1 * d0[IX(i1, j1, k1)]));
            }
          }
        }
      }
    }
  }
  set_bnd(M, N, O, b, d);
}

// Projection step to ensure incompressibility (make the velocity field
// divergence-free)
// OPTIMIZED !!!
void project(int M, int N, int O, float *u, float *v, float *w, float *p, float *div)
{
  // Optimização, retirar a função max do ciclo
  int max1 = MAX(M, MAX(N, O));

  // Tiling Technique
  int tile_size = 16;
#pragma omp parallel for schedule(dynamic)
  for (int jj = 1; jj <= N; jj += tile_size)
  {
    for (int kk = 1; kk <= O; kk += tile_size)
    {
      int block_Limit_J = (jj + tile_size < N + 1) ? jj + tile_size : N + 1;
      int block_Limit_K = (kk + tile_size < O + 1) ? kk + tile_size : O + 1;
      for (int ii = 1; ii <= M; ii += tile_size)
      {
        int block_Limit_I = (ii + tile_size < M + 1) ? ii + tile_size : M + 1;
        // Compute divergence and initialize pressure
        for (int j = jj; j < block_Limit_J; j++)
        {
          for (int k = kk; k < block_Limit_K; k++)
          {
            for (int i = ii; i < block_Limit_I; i++)
            {
              div[IX(i, j, k)] =
                  -0.5f * (u[IX(i + 1, j, k)] - u[IX(i - 1, j, k)] + v[IX(i, j + 1, k)] - v[IX(i, j - 1, k)] + w[IX(i, j, k + 1)] - w[IX(i, j, k - 1)]) / max1;
              p[IX(i, j, k)] = 0.0f;
            }
          }
        }
      }
    }
  }

  set_bnd(M, N, O, 0, div);
  set_bnd(M, N, O, 0, p);
  lin_solve(M, N, O, 0, p, div, 1, 6); // Solving for pressure using linear solver

#pragma omp parallel for schedule(dynamic)
  for (int jj = 1; jj <= N; jj += tile_size)
  {
    for (int kk = 1; kk <= O; kk += tile_size)
    {
      int block_Limit_J = (jj + tile_size < N + 1) ? jj + tile_size : N + 1;
      int block_Limit_K = (kk + tile_size < O + 1) ? kk + tile_size : O + 1;
      for (int ii = 1; ii <= M; ii += tile_size)
      {
        int block_Limit_I = (ii + tile_size < M + 1) ? ii + tile_size : M + 1;
        // Adjust velocities based on pressure gradient
        for (int j = jj; j < block_Limit_J; j++)
        {
          for (int k = kk; k < block_Limit_K; k++)
          {
            for (int i = ii; i < block_Limit_I; i++)
            {
              u[IX(i, j, k)] -= 0.5f * (p[IX(i + 1, j, k)] - p[IX(i - 1, j, k)]);
              v[IX(i, j, k)] -= 0.5f * (p[IX(i, j + 1, k)] - p[IX(i, j - 1, k)]);
              w[IX(i, j, k)] -= 0.5f * (p[IX(i, j, k + 1)] - p[IX(i, j, k - 1)]);
            }
          }
        }
      }
    }
  }
  set_bnd(M, N, O, 1, u);
  set_bnd(M, N, O, 2, v);
  set_bnd(M, N, O, 3, w);
}

// Step function for density
void dens_step(int M, int N, int O, float *x, float *x0, float *u, float *v,
               float *w, float diff, float dt)
{
  add_source(M, N, O, x, x0, dt);
  SWAP(x0, x);
  diffuse(M, N, O, 0, x, x0, diff, dt);
  SWAP(x0, x);
  advect(M, N, O, 0, x, x0, u, v, w, dt);
}

// Step function for velocity
void vel_step(int M, int N, int O, float *u, float *v, float *w, float *u0,
              float *v0, float *w0, float visc, float dt)
{
  add_source(M, N, O, u, u0, dt);
  add_source(M, N, O, v, v0, dt);
  add_source(M, N, O, w, w0, dt);
  SWAP(u0, u);
  diffuse(M, N, O, 1, u, u0, visc, dt);
  SWAP(v0, v);
  diffuse(M, N, O, 2, v, v0, visc, dt);
  SWAP(w0, w);
  diffuse(M, N, O, 3, w, w0, visc, dt);
  project(M, N, O, u, v, w, u0, v0);
  SWAP(u0, u);
  SWAP(v0, v);
  SWAP(w0, w);
  advect(M, N, O, 1, u, u0, u0, v0, w0, dt);
  advect(M, N, O, 2, v, v0, u0, v0, w0, dt);
  advect(M, N, O, 3, w, w0, u0, v0, w0, dt);
  project(M, N, O, u, v, w, u0, v0);
}
