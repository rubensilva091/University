import React from 'react';
import { View, Text, StyleSheet, Alert, useColorScheme, Image, Pressable } from 'react-native';
import { signOut } from 'firebase/auth';
import { auth } from '../constants/firebaseConfig';
import { useRouter } from 'expo-router';

// Import the profile image (adjust the path if necessary)
const profileIcon = require('../assets/images/profile.jpg');

export default function ProfileScreen() {
  const router = useRouter();
  const colorScheme = useColorScheme();

  const handleLogout = async () => {
    try {
      await signOut(auth);
      router.replace('/login');
    } catch (error) {
      Alert.alert('Erro', error.message);
    }
  };

  const handleGoBackToMap = () => {
    router.replace('/(tabs)/');
  };

  return (
    <View style={[styles.container, { backgroundColor: '#f2f2f2' }]}>
      <View style={[styles.card, { backgroundColor: '#fff' }]}>
        <Text style={[styles.title, { color: colorScheme === 'dark' ? '#000000' : '#000' }]}>Perfil</Text>
        <Image source={profileIcon} style={styles.profileImage} />
        <Text style={[styles.text, { fontWeight: 'bold', color: colorScheme === 'dark' ? '#000000' : '#333' }]}>Email:</Text>
        <Text style={[styles.email, { color: colorScheme === 'dark' ? '#555' : '#555' }]}>{auth.currentUser?.email}</Text>
        <View style={styles.buttonsContainer}>
          <Pressable style={styles.button} onPress={handleGoBackToMap}>
            <Text style={styles.buttonText}>Voltar</Text>
          </Pressable>
          <Pressable style={[styles.button, { backgroundColor: 'red' }]} onPress={handleLogout}>
            <Text style={styles.buttonText}>Logout</Text>
          </Pressable>
        </View>
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    padding: 20,
    backgroundColor: '#f2f2f2',
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
    justifyContent: 'center',
    alignItems: 'center',
  },
  title: {
    fontSize: 28,
    fontWeight: 'bold',
    marginBottom: 20,
    textAlign: 'center',
  },
  profileImage: {
    width: 100,
    height: 100,
    borderRadius: 50,
    marginBottom: 20,
  },
  text: {
    fontSize: 18,
    marginTop: 10,
    textAlign: 'left',
  },
  email: {
    fontSize: 16,
    marginTop: 5,
    fontWeight: '500',
    textAlign: 'left',
  },
  buttonsContainer: {
    marginTop: 30,
    width: '100%',
  },
  button: {
    marginVertical: 10,
    backgroundColor: '#007AFF',
    paddingVertical: 12,
    borderRadius: 8,
  },
  buttonText: {
    color: '#fff',
    textAlign: 'center',
    fontWeight: 'bold',
    fontSize: 16,
  },
});