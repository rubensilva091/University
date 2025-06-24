import React, { useState, useEffect } from 'react';
import { StyleSheet, View, Dimensions, Text, Button, Modal, Pressable } from 'react-native';
import MapView, { Marker, Circle } from 'react-native-maps';
import * as Location from 'expo-location';
import { useRouter, useLocalSearchParams } from 'expo-router';
import { auth } from '../../constants/firebaseConfig';
import uminho_locations from '../../assets/uminho_locations.json';
import tricornio from '../../assets/images/tricornio_emoji.png';
import { addVisitedBuilding, getVisitedBuildings, saveBadge, getUserBadges } from '../../constants/firebaseHelpers';
import { allBadges } from '../../constants/badges';
import { Alert } from 'react-native';
import BarometerComponent from '../barometer';
import { CompassIcon } from '../bussola';
import { Finder } from '../finder';

export default function TabOneScreen() {
  const [location, setLocation] = useState<any>(null);
  const [errorMsg, setErrorMsg] = useState(null);
  const [enteredBuildings, setEnteredBuildings] = useState([]);
  const [popupBuilding, setPopupBuilding] = useState(null);
  const [visitedBuildings, setVisitedBuildings] = useState<string[]>([]);
  const [unlockedBadges, setUnlockedBadges] = useState<Record<string, boolean>>({});
  const [isDataLoaded, setIsDataLoaded] = useState(false);
  const [finderActive, setFinderActive] = useState(false);
  const [destination, setDestination] = useState(null);
  const router = useRouter();
  const params = useLocalSearchParams();
  const destinationId = params.destinationId;

  useEffect(() => {
    const unsubscribe = auth.onAuthStateChanged((user) => {
      if (!user) {
        router.replace('/login');
      }
    });
    return unsubscribe;
  }, []);

  useEffect(() => {
    (async () => {
      try {
        const badges = await getUserBadges();
        setUnlockedBadges(badges);
        const visited = await getVisitedBuildings();
        setVisitedBuildings(visited);
        setIsDataLoaded(true);

        let { status } = await Location.requestForegroundPermissionsAsync();
        if (status !== 'granted') {
          setErrorMsg('Permissão de localização negada!');
          return;
        }

        let currentLocation = await Location.getCurrentPositionAsync({
          accuracy: Location.Accuracy.High,
          timeInterval: 5000,
          distanceInterval: 10,
        });
        setLocation(currentLocation);

        Location.watchPositionAsync(
          {
            accuracy: Location.Accuracy.High,
            timeInterval: 5000,
            distanceInterval: 10,
          },
          (newLocation) => {
            setLocation(newLocation);
          }
        );
      } catch (error) {
        console.error('Error in initial setup:', error);
        setErrorMsg('Erro ao carregar dados.');
      }
    })();
  }, []);

  // Lidar com o destinationId vindo da navegação
  useEffect(() => {
    if (destinationId) {
      const building = uminho_locations.find(b => b.id === destinationId);
      if (building) {
        setDestination(building);
        setFinderActive(true);
      }
    }
  }, [destinationId]);

  useEffect(() => {
    if (location && isDataLoaded) {
      checkGeofences();
    }
  }, [location, isDataLoaded]);

  const checkGeofences = async () => {
    const { latitude, longitude } = location.coords;
    const newEnteredBuildings = [...enteredBuildings];

    for (const building of uminho_locations) {
      const distance = calculateDistance(
        latitude, longitude,
        building.latitude, building.longitude
      );

      const isCurrentlyInside = distance < building.radius;
      const wasPreviouslyInside = enteredBuildings.includes(building.id);

      if (isCurrentlyInside && !wasPreviouslyInside) {
        setPopupBuilding(building);

        if (!visitedBuildings.includes(building.id)) {
          await addVisitedBuilding(building.id);
          setVisitedBuildings((prev) => [...prev, building.id]);
          checkBadges([...visitedBuildings, building.id]);
        }

        newEnteredBuildings.push(building.id);
      } else if (!isCurrentlyInside && wasPreviouslyInside) {
        newEnteredBuildings.splice(newEnteredBuildings.indexOf(building.id), 1);
      }
    }

    setEnteredBuildings(newEnteredBuildings);
  };

  const calculateDistance = (lat1, lon1, lat2, lon2) => {
    const R = 6371;
    const dLat = deg2rad(lat2 - lat1);
    const dLon = deg2rad(lon2 - lon1);
    const a =
      Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) *
      Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    const d = R * c * 1000;
    return d;
  };

  const deg2rad = (deg) => deg * (Math.PI / 180);

  const checkBadges = async (currentVisitedBuildings: string[]) => {
    const context = { visitedBuildings: currentVisitedBuildings };

    for (const badge of allBadges) {
      const conditionMet = badge.condition(context);
      if (conditionMet && !unlockedBadges[badge.id]) {
        await saveBadge(badge.id);
        setUnlockedBadges((prev) => ({ ...prev, [badge.id]: true }));
        Alert.alert('Nova conquista!', `${badge.title}\n${badge.description}`);
      }
    }
  };

  let mapRegion = {
    latitude: 41.5579,
    longitude: -8.4025,
    latitudeDelta: 0.02,
    longitudeDelta: 0.02,
  };

  if (location) {
    mapRegion = {
      latitude: location.coords.latitude,
      longitude: location.coords.longitude,
      latitudeDelta: 0.005,
      longitudeDelta: 0.005,
    };
  }

  const handleGoToFAQ = () => {
    router.push('/faq');
  };

  const handleFinderPress = () => {
    if (finderActive && destination === uminho_locations[0]) {
      setFinderActive(false);
      setDestination(null);
    } else {
      setDestination(uminho_locations[0]);
      setFinderActive(true);
    }
  };

  return (
    <View style={styles.container}>
      <View style={styles.profileButton}>
        <Button title="Perfil" onPress={() => router.push('/profile')} />
      </View>
      <View style={styles.badgesButton}>
        <Button title="Ver Conquistas" onPress={() => router.push('/badges')} />
      </View>


      {location && (
        <BarometerComponent
          currentLocation={{ latitude: location.coords.latitude, longitude: location.coords.longitude }}
        />
      )}

      <MapView
        style={styles.map}
        initialRegion={mapRegion}
        showsUserLocation={false}
        onPress={() => setPopupBuilding(null)}
      >
        {uminho_locations.map((building) => (
          <React.Fragment key={building.id}>
            <Marker
              coordinate={building}
              title={building.name}
              description={building.description}
              onPress={() => router.push(`/building/${building.id}`)}
            />
            <Circle
              center={building}
              radius={building.radius}
              fillColor="rgba(200, 200, 200, 0.3)"
              strokeColor="rgba(0, 0, 0, 0.3)"
            />
          </React.Fragment>
        ))}
        {location && (
          <Marker
            coordinate={{
              latitude: location.coords.latitude,
              longitude: location.coords.longitude,
            }}
            anchor={{ x: 0.5, y: 0.5 }}
          >
            <CompassIcon source={tricornio} />
          </Marker>
        )}
      </MapView>

      {popupBuilding && (
        <Modal
          animationType="slide"
          transparent={true}
          visible={true}
          onRequestClose={() => setPopupBuilding(null)}
        >
          <View style={styles.popup}>
            <Text style={styles.infoName}>Entrou em {popupBuilding.name}</Text>
            <Text style={styles.infoDescription}>{popupBuilding.description}</Text>
            <Button
              title="Ver detalhes"
              onPress={() => {
                setPopupBuilding(null);
                router.push(`/building/${popupBuilding.id}`);
              }}
            />
            <Button title="Fechar" onPress={() => setPopupBuilding(null)} />
          </View>
        </Modal>
      )}

      <Text style={styles.locationText}>
        {errorMsg ? errorMsg : location ? `Latitude: ${location.coords.latitude}, Longitude: ${location.coords.longitude}` : "A obter localização..."}
      </Text>

      <Pressable style={styles.helpButton} onPress={handleGoToFAQ}>
        <Text style={styles.helpButtonText}>?</Text>
      </Pressable>

      {finderActive && destination && location && (
        <Finder
          currentLocation={location.coords}
          destination={destination}
          fullScreen={!!destinationId} // Ativa tela cheia se vier de "find me"
          onClose={() => {
            setFinderActive(false);
            setDestination(null);
            router.setParams({ destinationId: null }); // Limpa o parâmetro
          }}
        />
      )}
    </View>
  );
}

const { width, height } = Dimensions.get('window');

const styles = StyleSheet.create({
  container: { flex: 1 },
  map: { flex: 1 },
  locationText: { textAlign: 'center', marginVertical: 10, fontSize: 16 },
  popup: {
    position: 'absolute',
    bottom: 50,
    left: 20,
    right: 20,
    backgroundColor: 'white',
    padding: 20,
    borderRadius: 10,
    elevation: 5,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.25,
    shadowRadius: 3.84,
  },
  infoName: { fontSize: 18, fontWeight: 'bold', marginBottom: 10 },
  infoDescription: { fontSize: 14, marginBottom: 10 },
  profileButton: { position: 'absolute', top: 40, right: 20, zIndex: 1 },
  badgesButton: { position: 'absolute', top: 40, left: 20, zIndex: 1 },
  finderButton: { position: 'absolute', top: 100, right: 20, zIndex: 1 },
  helpButton: {
    position: 'absolute',
    bottom: 5,
    left: 5,
    backgroundColor: 'white',
    borderRadius: 50,
    width: 35,
    height: 35,
    justifyContent: 'center',
    alignItems: 'center',
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.25,
    shadowRadius: 3.84,
    zIndex: 30,
  },
  helpButtonText: {
    fontSize: 20,
    color: 'black',
    fontWeight: 'bold',
  },
});