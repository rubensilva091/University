# Association Management System

**Master's Final Project**

This repository contains the source code for a comprehensive **Association Management System**. Designed to modernize administrative processes for clubs and associations, this full-stack web application bridges the gap between administrators and members through digitization.

## 📋 Project Overview

The platform provides a centralized solution for managing members, processing payments, and organizing events. It replaces manual paperwork with a responsive web interface and a robust backend API.

### Key Features

* **👥 Member Management:**
    * User registration and profile updates.
    * Digital Membership Card generation.
    * Role-based access (Administrator vs. Member).
* **💰 Financial Management:**
    * **Quotas:** Automated calculation and tracking of member dues.
    * **Payments:** Integration with Portuguese payment gateways (**Multibanco** and **MB WAY**).
    * Real-time payment history and status tracking.
* **🎟️ Event Management:**
    * Event creation and publishing.
    * Ticket generation (PDF) with unique **QR Codes**.
    * **Access Control:** built-in scanner feature to validate tickets at the door.
* **📧 Notifications:** Automated emails for registration, payments, and tickets.

## 🛠 Tech Stack

The project is built using a microservices-ready architecture:

### Backend
* **Language:** Go (Golang)
* **Framework:** Gin Gonic
* **Database:** PostgreSQL
* **Auth:** JWT (JSON Web Tokens) with RSA encryption
* **Key Libs:** `pgx` (Database driver), PDF generation, SMTP mailer.

### Frontend
* **Framework:** React.js
* **State Management:** Redux & Redux-Saga
* **Styling:** CSS Modules & Custom Components
* **Server:** Caddy (for production serving)

## 🚀 Getting Started

Follow these instructions to set up the project locally for development.

### Prerequisites
* [Go](https://go.dev/) (v1.19+)
* [Node.js](https://nodejs.org/) (v16+) & npm
* [PostgreSQL](https://www.postgresql.org/)
* [Docker](https://www.docker.com/) (Optional)

### Installation

#### 1. Database Setup
Ensure you have a PostgreSQL instance running. Create a database for the project.

#### 2. Backend Setup
Navigate to the backend directory:

```bash
cd PI-backend-main
