1. **Create Virtual Environment:**
   ```bash
   python3 -m venv .venv
   source .venv/bin/activate 
   ```

2. **Install Dependencies**
    ```
    pip install -r requirements.txt
    ```

3. **Running the Server**
    ```
    python server.py
    ```

5. **Register**
    ```
    python client.py register --username alice
    ```

6. **Upload a file**
    ```
    python client.py upload --path security.md
    ```

7. **List**
    ```
    python client.py list
    ```

8. **Donwload File**
    ```
    python client.py download --file-id <FILE_ID> --out downloaded.txt
    ```

9. **Delete a file:**
    ```
    python client.py delete --file-id <FILE_ID>
    ```

10. **Test Everything**   
    ```
    python tests.py
    ```