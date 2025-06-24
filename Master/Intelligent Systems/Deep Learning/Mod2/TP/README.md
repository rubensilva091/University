
# OSS 2025 – Avaliação de Habilidades em Sutura

Este repositório contém a solução desenvolvida para o desafio OSS 2025, focado na avaliação automática de habilidades cirúrgicas em procedimentos de sutura.

## 📁 Estrutura do Repositório

- `data/` – Contém os vídeos originais, frames extraídos, imagens pré-processadas e os ficheiros de labels utilizados para treino e avaliação dos modelos.
- `notebooks/` – Notebooks Jupyter com o desenvolvimento e testes dos modelos para as diferentes tarefas.
- `README.md` – Este arquivo de documentação.

## 🧠 Tarefas Abordadas

1. **Task 1 – Classificação do Global Rating Score (GRS):** Classificação do desempenho global do cirurgião em categorias como iniciante, intermediário, proficiente e especialista.
2. **Task 2 – Avaliação dos Critérios OSATS:** Predição dos 8 critérios específicos do OSATS (Objective Structured Assessment of Technical Skills) para avaliação detalhada das habilidades técnicas.

## 🧰 Modelos Utilizados

Foram exploradas diversas arquiteturas de redes neurais, incluindo:

- Redes Convolucionais (CNNs) com diferentes profundidades.
- Modelos híbridos combinando CNNs com LSTM para captura de dependências temporais.
- Vision Transformers (ViT) para extração de características visuais avançadas.
- MLP para vetores de características extraídas.

## 📊 Avaliação

Os modelos foram avaliados utilizando métricas como:

- Accuracy.
- F1-score macro e ponderado.
- Matriz de confusão.

As avaliações foram realizadas nos conjuntos de validação e teste, com análise detalhada por critério no caso da Task 2.

## 📄 Relatório

O relatório completo do projeto está disponível no arquivo `relatorio.pdf`, contendo detalhes sobre a metodologia, experimentos e resultados obtidos.

## 📎 Referências

- Desafio OSS 2025: [Synapse OSS Challenge](https://www.synapse.org/Synapse:syn66256386/wiki/631726)
