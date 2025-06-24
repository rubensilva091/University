import React, { useEffect, useState } from 'react';
import { Barometer } from 'expo-sensors';
import { Text, View, Dimensions, Modal, TextInput, Pressable, Button } from 'react-native';
import uminho_locations from '../assets/uminho_locations.json';
import { Ionicons } from '@expo/vector-icons';

interface BarometerProps {
  currentLocation: { latitude: number; longitude: number } | null;
}

const { height } = Dimensions.get('window');

const rho_g = 0.1176; // hPa/m
const floor_height = 4; // meters

const BarometerComponent: React.FC<BarometerProps> = ({ currentLocation }) => {
  const [pressure, setPressure] = useState<number | null>(null);
  const [currentBuilding, setCurrentBuilding] = useState<any>(null);
  const [floor, setFloor] = useState<number | null>(null);
  const [modalVisible, setModalVisible] = useState(false);
  const [inputFloor, setInputFloor] = useState('');
  const [calibrationData, setCalibrationData] = useState<{ [key: string]: { floor: number; pressure: number } }>({});

  useEffect(() => {
    let lastUpdate = 0;
    const subscription = Barometer.addListener(({ pressure }) => {
      const now = Date.now();
      if (now - lastUpdate >= 5000) {
        //console.log('Pressão atual:', pressure);
        setPressure(pressure);
        lastUpdate = now;
      }
    });
    return () => subscription.remove();
  }, []);

  useEffect(() => {
    if (currentLocation) {
      const building = findBuilding(currentLocation);
      //console.log('Edifício encontrado:', building);
      setCurrentBuilding(building);
    }
  }, [currentLocation]);

  useEffect(() => {
    if (pressure && currentBuilding) {
      const calibration = calibrationData[currentBuilding.id];
      if (calibration) {
        const h_diff = (calibration.pressure - pressure) / rho_g;
        const floorNumber = calibration.floor + Math.round(h_diff / floor_height);
        setFloor(floorNumber);
        //console.log(`Calculando com calibração: h_diff=${h_diff}, floorNumber=${floorNumber}`);
      } else {
        const base_pressure = currentBuilding.base_pressure;
        const h = (base_pressure - pressure) / rho_g;
        const raw_floor = h / floor_height;
        const floorNumber = Math.round(raw_floor) + 1;
        setFloor(floorNumber);
        //console.log(`Calculando sem calibração: base_pressure=${base_pressure}, h=${h}, raw_floor=${raw_floor}, floorNumber=${floorNumber}`);
      }
    } else {
      setFloor(null);
    }
  }, [pressure, currentBuilding, calibrationData]);

  const findBuilding = (location: { latitude: number; longitude: number }) => {
    return uminho_locations.find((building) => {
      const distance = calculateDistance(location, building);
      //console.log(`Distância para ${building.name}: ${distance} metros`);
      return distance < building.radius;
    });
  };

  const calculateDistance = (loc1: { latitude: number; longitude: number }, loc2: { latitude: number; longitude: number }) => {
    const R = 6371;
    const dLat = deg2rad(loc2.latitude - loc1.latitude);
    const dLon = deg2rad(loc2.longitude - loc1.longitude);
    const a =
      Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos(deg2rad(loc1.latitude)) * Math.cos(deg2rad(loc2.latitude)) *
      Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    return R * c * 1000;
  };

  const deg2rad = (deg: number) => deg * (Math.PI / 180);

  const handleConfirm = () => {
    if (inputFloor && currentBuilding && pressure) {
      const userFloor = parseInt(inputFloor, 10);
      if (isNaN(userFloor)) {
        //console.log('Piso inválido inserido');
        return;
      }
      setCalibrationData(prev => ({ ...prev, [currentBuilding.id]: { floor: userFloor, pressure } }));
      setModalVisible(false);
      setInputFloor('');
      //console.log(`Calibração para ${currentBuilding.id}: floor=${userFloor}, pressure=${pressure}`);
    }
  };

  return (
    <View
      style={{
        position: 'absolute',
        top: height * 0.92,
        left: 0,
        right: 0,
        alignItems: 'center',
        padding: 10,
        zIndex: 10,
      }}
    >
      {currentBuilding && floor !== null ? (
        <Text style={{ fontSize: 18, fontWeight: 'bold', color: 'lightblue' }}>
          {currentBuilding.name} - Piso {floor}
        </Text>
      ) : (
        <Text style={{ fontSize: 18, color: 'lightblue' }}>
          {currentBuilding ? '--------' : ''}
        </Text>
      )}

      <Pressable
        style={{
          position: 'absolute',
          bottom: 10,
          right: 10,
          backgroundColor: 'transparent',
        }}
        onPress={() => setModalVisible(true)}
      >
        <Ionicons name="refresh" size={24} color="lightblue" />
      </Pressable>

      <Modal
        animationType="slide"
        transparent={true}
        visible={modalVisible}
        onRequestClose={() => setModalVisible(false)}>
        <View style={{ flex: 1, justifyContent: 'center', alignItems: 'center', backgroundColor: 'rgba(0,0,0,0.5)' }}>
          <View style={{ backgroundColor: 'white', padding: 20, borderRadius: 10, width: 300 }}>
            <Text style={{ fontSize: 16, marginBottom: 10 }}>Insira o número do piso atual para recalibrar o Barômetro:</Text>
            <TextInput
              style={{ height: 40, borderColor: 'gray', borderWidth: 1, marginBottom: 10, paddingHorizontal: 10 }}
              keyboardType="numeric"
              value={inputFloor}
              onChangeText={setInputFloor}
            />
            <View style={{ flexDirection: 'row', justifyContent: 'space-between' }}>
              <Button title="Confirmar" onPress={handleConfirm} />
              <Button title="Cancelar" onPress={() => setModalVisible(false)} />
            </View>
          </View>
        </View>
      </Modal>
    </View>
  );
};

export default BarometerComponent;