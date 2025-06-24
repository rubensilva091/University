import { useLocalSearchParams, useRouter } from 'expo-router';
import { View, Text, StyleSheet, Linking, Pressable, Button } from 'react-native';
import uminho_locations from '../../assets/uminho_locations.json';

export default function BuildingDetails() {
  const params = useLocalSearchParams();
  const router = useRouter();
  const buildingId = Array.isArray(params.id) ? params.id[0] : params.id;

  const building = uminho_locations.find((b) => String(b.id) === String(buildingId));

  if (!building) {
    return (
      <View style={styles.container}>
        <Text style={styles.notFound}>Edifício não encontrado 😢</Text>
      </View>
    );
  }

  const openPlanta = () => {
    if (building.planta) {
      Linking.openURL(building.planta);
    }
  };

  const handleFindMe = () => {
    // Navega para a tela principal com o destino definido
    router.push({
      pathname: '/',
      params: { destinationId: building.id },
    });
  };

  // Função para formatar os horários e separar por local
  const formatHorario = (horario) => {
    const horariosArray = horario.split(',');
    return horariosArray.map((item, index) => {
      const [local, time] = item.split(':');
      return (
        <View key={index} style={styles.horarioContainer}>
          <Text style={styles.local}>{local.trim()}</Text>
          <Text style={styles.time}>{time.trim()}</Text>
        </View>
      );
    });
  };

  return (
    <View style={styles.container}>
      <View style={styles.card}>
        <Text style={styles.name}>{building.name}</Text>
        <Text style={styles.description}>{building.description}</Text>

        {/* Verifica se o horário está disponível e exibe se houver */}
        {building.horario ? (
          <>
            <Text style={styles.horarioTitle}>Horário:</Text>
            {formatHorario(building.horario)}
          </>
        ) : null}

        {building.planta && (
          <Pressable style={styles.button} onPress={openPlanta}>
            <Text style={styles.buttonText}>Ver planta</Text>
          </Pressable>
        )}

        {/* Botão "Find me" adicionado */}
        <Pressable style={styles.button} onPress={handleFindMe}>
          <Text style={styles.buttonText}>Find me</Text>
        </Pressable>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#f2f2f2',
    justifyContent: 'center',
    alignItems: 'center',
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
    width: '100%',
    maxWidth: 400,
  },
  name: {
    fontSize: 24,
    fontWeight: 'bold',
    marginBottom: 12,
    color: '#333',
    textAlign: 'center',
  },
  description: {
    fontSize: 16,
    textAlign: 'justify',
    color: '#555',
    marginBottom: 16,
  },
  horarioTitle: {
    fontSize: 18,
    fontWeight: 'bold',
    marginTop: 20,
    color: '#333',
  },
  horarioContainer: {
    marginVertical: 5,
  },
  local: {
    fontSize: 16,
    fontWeight: 'bold',
    color: '#00000',
  },
  time: {
    fontSize: 16,
    color: '#555',
  },
  button: {
    marginTop: 10,
    backgroundColor: '#007AFF',
    paddingVertical: 10,
    borderRadius: 8,
  },
  buttonText: {
    color: '#fff',
    textAlign: 'center',
    fontWeight: 'bold',
    fontSize: 16,
  },
  notFound: {
    fontSize: 20,
    color: 'red',
  },
});