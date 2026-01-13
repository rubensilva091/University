# Association Management System

**Master's Final Project**

This repository contains the source code for an Association Management System, developed as part of a Master's degree curriculum. The project aims to digitize administrative processes for associations, providing a unified platform for member management, financial administration, and event organization.

## Project Overview

The solution consists of a full-stack web application designed to replace manual record-keeping with a centralized digital system. It features a responsive frontend for members and administrators, supported by a microservices-ready backend.

### Core Functionality

* **Membership Management:**
    * User registration, profile management, and data validation.
    * Role-based access control (Administrators and Standard Members).
    * Generation of digital membership cards.
* **Financial Administration:**
    * Automated calculation of membership quotas/dues.
    * Integration with Portuguese payment gateways (Multibanco and MB WAY).
    * Real-time payment status tracking and history logs.
* **Event Management:**
    * Event creation, publishing, and capacity management.
    * Ticket generation via PDF with unique QR Codes.
    * Ticket validation system (QR scanning) for event access control.
* **System Notifications:**
    * Automated transactional emails for account recovery, payment confirmations, and ticket delivery.

## Technical Architecture

The project is structured as a monorepo containing two main components:

### Backend
* **Language:** Go (Golang 1.19+)
* **Framework:** Gin Gonic
* **Database:** PostgreSQL
* **Authentication:** JWT (JSON Web Tokens) using RSA encryption.
* **Key Libraries:** `pgx` (Database driver), `gofpdf` (PDF generation), native SMTP implementation.

### Frontend
* **Framework:** React.js
* **State Management:** Redux & Redux-Saga
* **Styling:** CSS Modules
* **Server:** Caddy (configured for production serving)

## Prerequisites

Before running the project, ensure you have the following installed:

* Go (version 1.19 or higher)
* Node.js (version 16 or higher) and npm
* PostgreSQL
* Docker (optional, for containerized execution)

## Installation and Setup

### 1. Database Configuration
Ensure a PostgreSQL instance is running. The backend is designed to handle migrations, but the database schema must be initialized.

### 2. Backend Setup
Navigate to the backend directory:

**Configuration:**
Locate the `etc/npadmin.config` file to understand the required environment variables. You will need to configure:
* **Database Connection:** Set your PostgreSQL DSN (e.g., `host=localhost user=postgres password=...`).
* **SMTP Settings:** Credentials for the email service to handle notifications.
* **Security Keys:** Ensure the RSA keys (public/private) for JWT signing are present in the defined paths.

**Run the service:**
```bash
# Download Go modules
go mod download

# Run the application
go run cmd/app/main.go
