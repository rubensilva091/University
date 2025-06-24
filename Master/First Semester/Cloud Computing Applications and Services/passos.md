# Checkpoints Summary

## Checkpoint 1: Docker Image Creation

 1. Created the Dockerfile for the Moonshot application.

 2. Built the Docker image using the following command:
        
        docker build -t moonshot-app:latest
         
 3. Verified the Docker image:
        
        docker images

 4. Created an account and a repository in the Docker Hub tu uplaod the docker image:

        docker login

        docker tag moonshot-app:latest pexometro/moonshot-app:latest

        docker push pexometro/moonshot-app:latest

    
## Checkpoint 2: Deployment and Tests

### 1. Preparing GKE Cluster

   * Login with  google cloud :

         gcloud init
        
   * Created and configured the GKE cluster using the following command:

         ansible-playbook -i inventory/gcp.yml gke-cluster-create.yml -e 'ansible_python_interpreter=/home/vagrant/.checkpoints/bin/python3'

   * Verified that the cluster was running:

         kubectl get nodes


### 2. Deploying the Moonshot Application

   * Pushed the Docker image to Google Container Registry (GCR):

         docker tag moonshot-app:latest gcr.io/ascn-grupo23/moonshot-app:latest

         docker push gcr.io/ascn-grupo23/moonshot-app:latest

   * Created a deployment file for Moonshot and executed the deployment:

         kubectl apply -f moonshot-deploy.yml

   * Verified that the Moonshot pod and service were running:

         kubectl get pods
         kubectl get svc

### 3. Deploying PostgreSQL

   * Created a deployment file for PostgreSQL and executed the deployment:

         kubectl apply -f postgres-deploy.yml

   * Verified that the PostgreSQL pod and service were running:

         kubectl get pods
         kubectl get svc

### 4. Undeploying Resources
    
  * Created undeploy files for Moonshot and PostgreSQL.

  * Executed the following commands to remove deployments and services:

        kubectl delete -f moonshot-undeploy.yml
        kubectl delete -f postgres-undeploy.yml

   * Destroyed the GKE cluster:

         ansible-playbook -i inventory/gcp.yml gke-cluster-destroy.yml -e 'ansible_python_interpreter=/home/vagrant/.checkpoints/bin/python3'

   * Verified that all resources were deleted:

          kubectl get all





