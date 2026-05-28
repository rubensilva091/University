# System Architecture

The architecture of the **Secure File Storage** system is designed to enforce strict security boundaries, ensuring multi-tenancy isolation and supporting a "Full Privacy Mode". The system follows a client-server model, utilizing a RESTful API and a lightweight terminal client.

## Architecture Diagram

Below is the high-level architecture diagram representing the main components, their environments, and the data flow between them:

```mermaid
flowchart TD
    %% External Entities
    U(User)
    OIDC[OIDC Provider \n e.g., Google]

    %% Client Side
    subgraph Client_Side [Client Environment]
        CLI[CLI Client \n Python]
        KS[(Local Key Store \n SQLite/File)]
    end

    %% Server Side
    subgraph Server_Side [Backend Environment / Docker]
        API[FastAPI Server]
        DB[(Metadata DB \n SQLite)]
        FS[File Storage \n Local Disk / AWS S3]
        LOG[(Audit Logs / \n Centralized Logging)]
    end

    %% Connections
    U -->|Uses| CLI
    CLI -.->|Authentication| OIDC
    CLI ==|REST API \n HTTPS|==> API
    
    API -->|Reads/Writes Metadata| DB
    API -->|Stores/Retrieves \n Encrypted Files| FS
    API -.->|Writes Audit Trails \n & Traces| LOG
    CLI -->|Stores DEK & Nonce| KS

    ```