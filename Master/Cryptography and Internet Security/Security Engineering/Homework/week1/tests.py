import requests
import unittest
import os

BASE_URL = "http://127.0.0.1:5000"

class TestFileVault(unittest.TestCase):

    def setUp(self):
        # Gera um user aleatório para cada teste não colidir
        self.username = f"test_user_{os.urandom(4).hex()}"
        # 1. Registar
        resp = requests.post(f"{BASE_URL}/register", json={"username": self.username})
        self.assertEqual(resp.status_code, 201)
        self.api_key = resp.json()['api_key']
        self.headers = {'Authorization': self.api_key}

    def test_full_flow(self):
        print(f"\nTesting flow for user: {self.username}")

        # 2. Upload
        filename = "test_file.txt"
        file_content = b"Conteudo de teste para a API."
        files = {'file': (filename, file_content)}
        
        print(" -> Uploading file...")
        resp = requests.post(f"{BASE_URL}/files", files=files, headers=self.headers)
        self.assertEqual(resp.status_code, 201)
        file_id = resp.json()['file_id']
        print(f"    Success. File ID: {file_id}")

        # 3. List
        print(" -> Listing files...")
        resp = requests.get(f"{BASE_URL}/files", headers=self.headers)
        self.assertEqual(resp.status_code, 200)
        data = resp.json()
        self.assertEqual(len(data['files']), 1)
        self.assertEqual(data['files'][0]['filename'], filename)
        print("    Success. File listed correctly.")

        # 4. Download
        print(" -> Downloading file...")
        resp = requests.get(f"{BASE_URL}/files/{file_id}", headers=self.headers)
        self.assertEqual(resp.status_code, 200)
        self.assertEqual(resp.content, file_content)
        print("    Success. Content matches.")

        # 5. Delete
        print(" -> Deleting file...")
        resp = requests.delete(f"{BASE_URL}/files/{file_id}", headers=self.headers)
        self.assertEqual(resp.status_code, 200)
        
        # Verificar se desapareceu
        resp = requests.get(f"{BASE_URL}/files", headers=self.headers)
        self.assertEqual(len(resp.json()['files']), 0)
        print("    Success. File deleted.")

if __name__ == '__main__':
    print("Running automated tests against local server...")
    unittest.main()