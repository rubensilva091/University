# Hands-on

## First part

The general goal of the first part is to experiment a bit and to generate 2 files: 1) `trivy-juice.json` and 2) `syft-juice.json` using `trivy` and `syft`. These two files are SBOMs from two docker images tipically used for security-related exercises.

1. Build the `Dockerfile.toolbox` (check Makefile)

2. Run `docker image ls` to list the images that you have

3. Run `docker ps -a` to list the containers (if don't use docker you should see nothing)
   - run `docker container prune -f` to remove no longer used containers (if you see something that should be removed)

4. Run `docker pull bkimminich/juice-shop:latest`

5. Run `docker pull vulnerables/web-dvwa:latest`

6. Run `docker run -d --name juice -p 3000:3000 bkimminich/juice-shop:latest`

7. Run `docker ps -a` and verify that the juice container is there

8. Open http://localhost:3000/ on your browser and see what it does

9. Kill it and remove the container (not the image) `docker kill juice && docker rm juice`

10. Run `docker run --rm -it --name toolbox -v "$PWD:/work" -v /var/run/docker.sock:/var/run/docker.sock -w /work ss2526-w3tb:latest`

11. Run `trivy image bkimminich/juice-shop:latest` (**read the output**)

12. Run `trivy image --format cyclonedx -o trivy-juice.json bkimminich/juice-shop:latest`

13. Run `syft bkimminich/juice-shop:latest -o cyclonedx-json > syft-juice.json`


## Second part

The goal of the second part is 1) validate the JSON files produced (to check if they are actually valid CicloneDX SBOM files); 2) use `grype` and `trivy` to list the vulnerabilities; 3) Compare how different tools report issue and know how to read them.

1. Run `wget https://github.com/CycloneDX/cyclonedx-cli/releases/download/v0.30.0/cyclonedx-linux-x64` or the corresponding version (check [https://github.com/CycloneDX/cyclonedx-cli/releases/tag/v0.30.0](https://github.com/CycloneDX/cyclonedx-cli/releases/tag/v0.30.0))

2. Run `chmod +x cyclonedx-linux-x64`

3. Run `./cyclonedx-linux-x64 validate --input-file syft-juice.json` (you should see "BOM validated successfully.")

4. Run `./cyclonedx-linux-x64 validate --input-file trivy-juice.json`  (you should see "BOM validated successfully.")

5. Run `grype sbom:syft-juice.json > grype__syft-juice.txt`

6. Run `grype sbom:trivy-juice.json > grype__trivy-juice.txt`

7. Run `trivy sbom syft-juice.json > trivy__syft-juice.txt`

8. Run `trivy sbom trivy-juice.json > trivy__trivy-juice.txt`

9. Compare `grype__syft-juice.txt` and `grype__trivy-juice.txt` (using `meld` for instance)

10. Compare `trivy__syft-juice.txt` and `trivy__trivy-juice.txt` (using `meld` for instance)

11. Take a look into a given `grype__*` file and then check the documentation in [https://oss.anchore.com/docs/guides/vulnerability/interpreting-results/](https://oss.anchore.com/docs/guides/vulnerability/interpreting-results/) to learn how to read the data and also learn about the different options for `grype`.

12. **What is a GHSA-xxxx-xxxx-xxxx? Should it be taken seriously? Is it related with CVEs?** 

**Answer P2.12:**

**GHSA (GitHub Security Advisory)**: é um identificador de vulnerabilidades específico da base de dados de segurança do GitHub, focado principalmente no ecossistema de pacotes open-source.

**Deve ser levado a sério?** Sim! Representam vulnerabilidades reais na *supply chain* que podem ser exploradas da mesma forma que outras falhas de segurança.

**Relação com CVEs**: Estão intimamente ligados. A grande maioria dos identificadores GHSA mapeia de forma direta para um CVE oficial. O formato GHSA existe porque o GitHub atua como uma autoridade, o que lhes permite emitir alertas e avisar a comunidade de programadores de forma muito mais rápida, muitas vezes antes do processo burocrático de atribuição de um CVE estar concluído.

---

13. Which combination seems to yield better results? 

**Answer P2.13:**

As combinações "nativas" tendem a produzir os melhores resultados e relatórios mais completos.

O formato do SBOM (CycloneDX) é standard, a forma como cada ferramenta preenche campos opcionais, formata os identificadores (CPEs/PURLs) e constrói a árvore de dependências varia ligeiramente. Quando usamos ferramentas do mesmo fabricante, o Grype sabe exatamente como interpretar os dados e as nuances do Syft, não havendo perda de contexto.

Por outro lado, ao cruzar ferramentas, é comum ocorrerem falhas no mapeamento de certos pacotes, o que resulta em vulnerabilidades que passam despercebidas (false negatives) ou ficheiros com menos detalhe.

---

## Third part

The goal of the third part is to become familiar with OSV.

1. Check [https://osv.dev/](https://osv.dev/)

2. Check [https://github.com/google/osv-scanner](https://github.com/google/osv-scanner)

3. Run `wget https://github.com/google/osv-scanner/releases/download/v2.3.3/osv-scanner_linux_amd64` (might need to change for different settings like arm64 cpus; **btw, it is OK to run wget like this?**)

4. Run `chmod +x osv-scanner_linux_amd64`

5. Run `./osv-scanner_linux_amd64 --help`

6. Run `./osv-scanner_linux_amd64 scan image --all-vulns --format vertical bkimminich/juice-shop:latest` (**experiment with different options**)

Did the previous command failed? Try fixing it with `apt update && apt install docker.io`

Start thinking on how to combine the outputs of different tools.

7. **btw, it is OK to run wget like this?** 

Não, é muito errado a nível de *Segurança*! Download de um ficheiro binário diretamente da internet com o *wget* e dar-lhe permissões de execução logo a seguir sem primeiro verificar a sua integridade é um risco! 
O correto seria fazer donwload também do ficheiro de checksums ou a assinatura fornecida pelos releases do repositório para garantir que o ficheiro não foi alterado.

---

## Fourth part (optional / if you still have some time)

1. Consider the file `docker_120_dataset.csv`;

2. Devise a way of a) test if the tag actually exists (something about docker manifest) and b) for all images that actually exist create a script, python or bash, that runs different tools on the images or the SBOMs corresponding to the images (btw, which one is more efficient, for monitoring purposes, assuming that the image only changes every (for example) 7 days).

**Resposta**

Para monitorização contínua, fazer scan ao SBOM é muito mais eficiente do que fazer scan à imagem.
Se a imagem só é atualizada a cada 7 dias, os seus componentes são estáticos durante esse período. Portanto, só se faz scan uma vez por semana. Nos restantes dias, faz-se apenas o scan ao SBOM contra as bases de dados de vulnerabilidades (que são alimentadas diariamente)!


Hope you had some fun.
yes :thumbsup:





