# Use the official Python image as the base image
FROM python:3.11.5

# Set environment variables
ARG TZ=US
ENV DEBIAN_FRONTEND=noninteractive
ENV SEED=false

# Install PostgreSQL common tools and Docker
RUN apt-get update && apt-get install -y postgresql-common 

# Set the working directory
WORKDIR /app

# Copy the application code
COPY moonshot moonshot
WORKDIR moonshot
RUN cp .env.example .env
COPY script.sh .

# Create a virtual environment and install dependencies
RUN pip3 install -r requirements.txt && \
    chmod +x ./script.sh

# Expose the application port
EXPOSE 8000

# Command to run the application
CMD ["./script.sh"]
