import csv
import subprocess
import sys

CSV_FILE = "docker_120_dataset.csv"

def main():
    try:
        # Abre o ficheiro CSV
        with open(CSV_FILE, mode='r', encoding='utf-8') as file:
            reader = csv.DictReader(file)
            
            for row in reader:
                full_image = row.get("full_image", "").strip()
                
                # Ignorar linhas vazias
                if not full_image:
                    continue

                print("-" * 50)
                print(f"A verificar se a imagem existe: {full_image}")

                # a) Testar se a tag existe usando o docker manifest
                manifest_check = subprocess.run(
                    ["docker", "manifest", "inspect", full_image],
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL
                )

                if manifest_check.returncode == 0:
                    print(f"A imagem '{full_image}' existe no Docker Hub.")
                    print("A gerar SBOM com o Trivy...")
                    
                    # Substituir barras e dois pontos por underscores para o nome do ficheiro
                    safe_name = full_image.replace("/", "_").replace(":", "_")
                    output_file = f"sbom_{safe_name}.json"
                    
                    # b) Correr a ferramenta (Trivy) na imagem
                    subprocess.run([
                        "trivy", "image", "--format", "cyclonedx", 
                        "-o", output_file, full_image
                    ])
                    
                    print(f"SBOM guardado como: {output_file}")
                
                    break 
                else:
                    print(f"A imagem '{full_image}' nao foi encontrada ou requer autenticacao.")

    except FileNotFoundError:
        print(f"Erro: O ficheiro '{CSV_FILE}' nao foi encontrado na diretoria atual.")
        sys.exit(1)

if __name__ == "__main__":
    main()