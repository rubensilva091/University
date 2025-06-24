#include "EventManager.h"
#include "fluid_solver.h"
#include <cuda_runtime.h>
#include <device_launch_parameters.h>
#include <iostream>
#include <vector>
#include <time.h>

#define SIZE 168

#define IX(i, j, k) ((i) + (M + 2) * (j) + (M + 2) * (N + 2) * (k))


static int M = SIZE;
static int N = SIZE;
static int O = SIZE;
static float dt = 0.1f;     
static float diff = 0.0001f; 
static float visc = 0.0001f; 


//Host
static float *u, *v, *w, *u_prev, *v_prev, *w_prev;
static float *dens, *dens_prev;
//Device
static float *d_u, *d_v, *d_w, *d_u_prev, *d_v_prev, *d_w_prev;
static float *d_dens, *d_dens_prev;



int allocate_data() {
  int size = (M + 2) * (N + 2) * (O + 2);
  u = new float[size];
  v = new float[size];
  w = new float[size];
  u_prev = new float[size];
  v_prev = new float[size];
  w_prev = new float[size];
  dens = new float[size];
  dens_prev = new float[size];

  if (!u || !v || !w || !u_prev || !v_prev || !w_prev || !dens || !dens_prev) {
    std::cerr << "Cannot allocate memory" << std::endl;
    return 0;
  }
  return 1;
}

void allocate_cuda()
{
  int size = (M + 2) * (N + 2) * (O + 2);
  cudaMalloc(&d_u, size * sizeof(float));
  cudaMalloc(&d_v, size * sizeof(float));
  cudaMalloc(&d_w, size * sizeof(float));
  cudaMalloc(&d_u_prev, size * sizeof(float));
  cudaMalloc(&d_v_prev, size * sizeof(float));
  cudaMalloc(&d_w_prev, size * sizeof(float));
  cudaMalloc(&d_dens, size * sizeof(float));
  cudaMalloc(&d_dens_prev, size * sizeof(float));
}

void memcpy_host_to_device()
{
  int size = (M + 2) * (N + 2) * (O + 2);
  cudaMemcpy(d_u, u, size * sizeof(float), cudaMemcpyHostToDevice);
  cudaMemcpy(d_v, v, size * sizeof(float), cudaMemcpyHostToDevice);
  cudaMemcpy(d_w, w, size * sizeof(float), cudaMemcpyHostToDevice);
  cudaMemcpy(d_u_prev, u_prev, size * sizeof(float), cudaMemcpyHostToDevice);
  cudaMemcpy(d_v_prev, v_prev, size * sizeof(float), cudaMemcpyHostToDevice);
  cudaMemcpy(d_w_prev, w_prev, size * sizeof(float), cudaMemcpyHostToDevice);
  cudaMemcpy(d_dens, dens, size * sizeof(float), cudaMemcpyHostToDevice);
  cudaMemcpy(d_dens_prev, dens_prev, size * sizeof(float), cudaMemcpyHostToDevice);
}

void memcpy_device_to_host()
{
  int size = (M + 2) * (N + 2) * (O + 2);
  cudaMemcpy(u, d_u, size * sizeof(float), cudaMemcpyDeviceToHost);
  cudaMemcpy(v, d_v, size * sizeof(float), cudaMemcpyDeviceToHost);
  cudaMemcpy(w, d_w, size * sizeof(float), cudaMemcpyDeviceToHost);
  cudaMemcpy(u_prev, d_u_prev, size * sizeof(float), cudaMemcpyDeviceToHost);
  cudaMemcpy(v_prev, d_v_prev, size * sizeof(float), cudaMemcpyDeviceToHost);
  cudaMemcpy(w_prev, d_w_prev, size * sizeof(float), cudaMemcpyDeviceToHost);
  cudaMemcpy(dens, d_dens, size * sizeof(float), cudaMemcpyDeviceToHost);
  cudaMemcpy(dens_prev, d_dens_prev, size * sizeof(float), cudaMemcpyDeviceToHost);
}

void free_cuda()
{
  cudaFree(d_u);
  cudaFree(d_v);
  cudaFree(d_w);
  cudaFree(d_u_prev);
  cudaFree(d_v_prev);
  cudaFree(d_w_prev);
  cudaFree(d_dens);
  cudaFree(d_dens_prev);
}


void clear_data() {
  int size = (M + 2) * (N + 2) * (O + 2);
  for (int i = 0; i < size; i++) {
    u[i] = v[i] = w[i] = u_prev[i] = v_prev[i] = w_prev[i] = dens[i] =
        dens_prev[i] = 0;
  }
}

void free_data() {
  delete[] u;
  delete[] v;
  delete[] w;
  delete[] u_prev;
  delete[] v_prev;
  delete[] w_prev;
  delete[] dens;
  delete[] dens_prev;
}


__global__ void apply_events_kernel(float* d_dens, float* d_u, float* d_v, float* d_w, 
                                  int i, int j, int k, float density, float fx, float fy, float fz, 
                                  int event_type, int M, int N, int O) {
    int index = IX(i, j, k);
    if (event_type == ADD_SOURCE) {
        d_dens[index] = density;
    } 
    else if (event_type == APPLY_FORCE) {
        d_u[index] = fx;
        d_v[index] = fy;
        d_w[index] = fz;
    }
}


void apply_events(const std::vector<Event>& events) {
    for (const auto& event : events) {
       
        int i = M / 2, j = N / 2, k = O / 2;
        
        apply_events_kernel<<<1, 1>>>(d_dens, d_u, d_v, d_w,
                                    i, j, k,
                                    event.density,
                                    event.force.x, event.force.y, event.force.z,
                                    event.type,
                                    M, N, O);
    }
}

void simulate(EventManager& eventManager, int timesteps) {
    memcpy_host_to_device(); 

    for (int t = 0; t < timesteps; t++) {
        
        std::vector<Event> events = eventManager.get_events_at_timestamp(t);
        apply_events(events);

        vel_step(M, N, O, d_u, d_v, d_w, d_u_prev, d_v_prev, d_w_prev, visc, dt);
        dens_step(M, N, O, d_dens, d_dens_prev, d_u, d_v, d_w, diff, dt);
    }
    
    memcpy_device_to_host();
}

float sum_density() {
  float total_density = 0.0f;
  int size = (M + 2) * (N + 2) * (O + 2);
  for (int i = 0; i < size; i++) {
    total_density += dens[i];
  }
  return total_density;
}

int main() {
  EventManager eventManager;
  eventManager.read_events("src/events.txt");

  int timesteps = eventManager.get_total_timesteps();

  if (!allocate_data())
    return -1;
  clear_data();
  allocate_cuda();


  clock_t start = clock();

  simulate(eventManager, timesteps);


  clock_t end = clock();
  std::cout << "Simulation took CPU " << (float)(end - start) / CLOCKS_PER_SEC << " seconds." << std::endl;
  float total_density = sum_density();
  std::cout << "Total density after " << timesteps
            << " timesteps: " << total_density << std::endl;

  free_data(); //Host
  free_cuda(); //Device

  return 0;
}
