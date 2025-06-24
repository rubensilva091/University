import React, { useState } from 'react';
import { View, Text, StyleSheet, Dimensions, ScrollView, TouchableOpacity, Linking } from 'react-native';
import Collapsible from 'react-native-collapsible';
import Icon from '@expo/vector-icons/FontAwesome';

export default function FAQ() {
  const [expandedIndex, setExpandedIndex] = useState(null);

  const faqData = [
    {
      question: "Aplicação SASUM?",
      answer: "A aplicação SASUM da Universidade do Minho, disponível para dispositivos Android e iOS, oferece aos estudantes acesso direto e simplificado a diversos serviços essenciais, como alimentação, alojamento, bolsas de estudo, apoio médico e psicológico, e atividades desportivas e culturais. Caso pretenda almoçar ou jantar na cantina, podes verificar os horários através da aplicação e deves fazer a reserva através desta. É também com esta aplicação que deves comprar senhas para o ginásio e usá-la para entrar no complexo desportivo."
    },
    {
      question: "Onde posso consultar o meu horário?",
      answer: "Os horários podem ser consultados em https://alunos.uminho.pt/pt/estudantes/paginas/infouteishorarios.aspx, ainda que, se o teu curso tiver um grande número de estudantes, podes ser dividido por turmas. Nesses casos, a tua direção de curso dar-te-á mais informações."
    },
    {
      question: "O que é a Blackboard?",
      answer: "A Blackboard é uma plataforma web de gestão e distribuição de informação e de recursos didáticos. Essencialmente é o local onde os docentes partilham os recursos necessários à unidade curricular. Além disso, ao selecionares uma unidade curricular, podes aceder aos contactos da equipa docente em \"Dossiê de UC\"."

    },
    {
      question: "Como acedo à Blackboard?",
      answer: "Pode ser acedida através do browser ou podes instalar a aplicação com as credenciais definidas na matrícula (número de aluno (ex: a12345) e palavra-passe). Será também com estas credenciais que irás ter acesso ao teu email institucional (através do Outlook), portal do aluno (alunos.uminho.pt) e à Eduroam."
    },
    {
      question: "O que é o email institucional?",
      answer: "É o meio formal de comunicação entre a comunidade académica. Esta caixa de email tem proteção anti-spam e antivírus e oferece armazenamento de 50GB e a capacidade de fazeres download das últimas versões do Office 365. Podes aceder-lhe a partir de outlook.office.com/mail/, ao inserires o teu email no formato axxxxx@alunos.uminho.pt, sendo axxxxx o teu número de aluno, e a palavra-passe definida na matrícula."
    },
    {
      question: "O que é a Eduroam?",
      answer: "É a rede wi-fi utilizada nos vários edifícios da Universidade do Minho. Caso precises de aceder remotamente a algum recurso que só está disponível na rede Eduroam, podes utilizar a VPN disponível."
    },
    {
      question: "Onde fica a minha sala de aula?",
      answer: "No teu horário tens uma sala associada a cada aula no formato edifício - piso.sala (exemplo: edifício 2 - 0.20 -> sala 20, piso 0, no edifício 2). Nos primeiros dias é normal sentires-te perdido no campus, pelo que podes procurar a tua sala em: https://campi.uminho.pt/."
    },
    {
      question: "Instalações de desporto na Universidade do Minho?",
      answer: "Sim! A UMinho Sports tem espaços nos Campus de Azurém, Gualtar e na Residência de Santa Tecla para que possas praticar desporto. Tens a possibilidade de alugar espaços desportivos e de frequentar as aulas disponíveis: http://www.sas.uminho.pt/Default.aspx?tabid=11&pageid=96&lang=pt-PT. Podes consultar a tabela de preços de serviços desportivos em: http://www.sas.uminho.pt/Default.aspx?tabid=11&pageid=50&lang=pt-PT. E também no Facebook: https://www.facebook.com/UMinhoSports/."
    },
    {
      question: "O que é a AAUMinho?",
      answer: "A AAUMinho (Associação Académica da Universidade do Minho) é o órgão representativo dos estudantes que promove atividades e iniciativas no âmbito da ação educativa e associativismo, ação social, cultura e tradições académicas. São exemplos de atividades organizadas pela AAUMinho: 1. Acolhimento 2. Cerimónia de Boas-Vindas do Reitor 3. GPS do Caloiro 4. Caloiro de Molho."
    },
    {
      question: "Porquê ser sócio da AAUMinho?",
      answer: "Ao seres sócio da AAUMinho tens acesso a descontos nos nossos parceiros e nas formações promovidas pela Start Point, e condições especiais na Receção ao Caloiro e no Enterro da Gata."
    },
    {
      question: "Há transporte na universidade?",
      answer: "A AAUMinho dispõe de um serviço de transporte que liga os dois campi de Gualtar e Azurém. Os bilhetes podem ser comprados nos espaços recurso ou através da aplicação Recurso. Consulta os horários mais atualizados em: https://www.aaum.pt/academia/transportes/."
    },
    {
      question: "Posso levar o meu carro para a universidade?",
      answer: "A UMinho tem parques de estacionamento disponíveis para os alunos. Para efetuares o pedido de acesso aos parques de estacionamento acede à tua área pessoal de aluno do Portal Académico, em Secretária Eletrónica > Parques."
    },
    {
      question: "Que atividades extra-curso existe?",
      answer: "Grupos culturais: https://www.aaum.pt/grupos-culturais/. Núcleos e secções: https://www.aaum.pt/nucleos-seccoes-e-delegacoes/."
    },
    {
      question: "Onde posso fazer refeições na universidade?",
      answer: "Estão disponíveis 3 cantinas em Gualtar, Santa Tecla e Azurém em que podes fazer uma refeição completa. As senhas individuais têm um custo de 2,70€ e um pack de 10 senhas tem um custo de 25€. Caso seja o prato simples, a senha tem um custo de 2,05€. As senhas podem ser adquiridas nos bares ou na cantina. Há ainda serviços de refeições não subsidiadas, como a Rampa B em Azurém (4,60€/senha), Grill em Azurém e Gualtar (6,50€/senha) e o Restaurante Panorâmico em Gualtar (11€/buffet ou 8€/prato do dia)."
    },
    {
      question: "Onde Estudar?",
      answer: "Tens várias bibliotecas disponíveis. Em particular, a biblioteca geral em Gualtar e a biblioteca de Azurém têm salas abertas 24h por dia, 7 dias por semana, além dos espaços restantes com horário menos livre. Além disso, podes reservar gabinetes, com um mínimo de 3 pessoas e máximo de 6 ou 8 pessoas; é aconselhado que dês check-in mal entres no gabinete: https://reservas.sdum.uminho.pt/Web/."
    },
    {
      question: "Que tipo de estatutos existem?",
      answer: "1. Trabalhador-Estudante — estatuto disponível para estudantes que trabalhem durante o período letivo. Este estatuto pode ser pedido no portal académico > pedidos > novo pedido > regimes especiais de frequência > trabalhador-estudante. 2. Estudante Dirigente Associativo — estatuto obtido ao ser eleito dirigente da AAUMinho. 3. Estudante Praticante Desportivo de Alto Rendimento — estudantes que constem do registo organizado pelo Instituto Português do Desporto e Juventude."
    },
    {
      question: "Posso colaborar com a Universidade?",
      answer: "Sim! Estas oportunidades são divulgadas no email institucional. É atribuída uma bolsa de acordo com o Regulamento Académico da Universidade do Minho no valor de 3€/hora. A colaboração não deve exceder 5 horas por dia e o máximo de 20 horas por semana."
    },
    {
      question: "Onde posso consultar mais informações?",
      answer: "Para mais informações, consulte os seguintes links: https://docs.google.com/document/u/0/d/1CsncjQUSLpUihC2fwJaBXXEGpFzKTbtvkwTI9j4_1JE/mobilebasic."
    }
  ];

  const toggleExpand = (index) => {
    setExpandedIndex(expandedIndex === index ? null : index);
  };

  return (
    <ScrollView style={styles.container}>
      <View style={styles.card}>
        <Text style={styles.title}>FAQ</Text>
        {faqData.map((item, index) => (
          <View key={index}>
            <TouchableOpacity onPress={() => toggleExpand(index)} style={styles.questionContainer}>
              <Text style={styles.question}>{item.question}</Text>
              <Icon name={expandedIndex === index ? "chevron-up" : "chevron-down"} size={20} color="darkblue" />
            </TouchableOpacity>
            <Collapsible collapsed={expandedIndex !== index}>
              <Text style={styles.answer}>{item.answer}</Text>
            </Collapsible>
          </View>
        ))}
      </View>
    </ScrollView>
  );
}

const { width, height } = Dimensions.get('window');
const marginPercentage = 0.05; // 5%
const leftMargin = width * marginPercentage;
const bottomMargin = height * marginPercentage;

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#f2f2f2',
    padding: 20,
  },
  card: {
    backgroundColor: '#fff',
    padding: 20,
    borderRadius: 16,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.25,
    shadowRadius: 4,
    elevation: 5,
  },
  title: {
    fontSize: 24,
    fontWeight: 'bold',
    marginBottom: 15,
    textAlign: 'center',
    color: '#333',
  },
  questionContainer: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    paddingVertical: 10,
  },
  question: {
    fontSize: 18,
    fontWeight: 'bold',
    color: '#0497db',
  },
  answer: {
    fontSize: 16,
    marginBottom: 20,
    color: '#555',
  },
});