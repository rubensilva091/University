import React, { useState } from 'react';
import { View, TextInput, Button, StyleSheet, Text, Alert, useColorScheme } from 'react-native';
import { signInWithEmailAndPassword } from 'firebase/auth';
import { auth } from '../constants/firebaseConfig';
import { useRouter } from 'expo-router';

export default function LoginScreen() {
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const router = useRouter();
  const colorScheme = useColorScheme();

  const handleLogin = async () => {
    try {
      await signInWithEmailAndPassword(auth, email, password);
      router.replace('/(tabs)/'); // Redireciona para as tabs depois de logar
    } catch (error: any) {
      Alert.alert('Erro', error.message);
    }
  };

  return (
    <View style={[styles.container, { backgroundColor: colorScheme === 'dark' ? '#000' : '#fff' }]}>
      <Text style={[styles.title, { color: colorScheme === 'dark' ? '#fff' : '#000' }]}>Login</Text>

      <TextInput
        placeholder="Email"
        placeholderTextColor={colorScheme === 'dark' ? '#aaa' : '#555'}
        value={email}
        onChangeText={setEmail}
        style={[styles.input, { backgroundColor: colorScheme === 'dark' ? '#222' : '#eee', color: colorScheme === 'dark' ? '#fff' : '#000' }]}
      />

      <TextInput
        placeholder="Password"
        placeholderTextColor={colorScheme === 'dark' ? '#aaa' : '#555'}
        value={password}
        onChangeText={setPassword}
        secureTextEntry
        style={[styles.input, { backgroundColor: colorScheme === 'dark' ? '#222' : '#eee', color: colorScheme === 'dark' ? '#fff' : '#000' }]}
      />

      <Button title="Login" onPress={handleLogin} />
      <View style={{ height: 10 }} />
      <Button title="Não tens conta? Regista-te" onPress={() => router.push('/register')} />
    </View>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, justifyContent: 'center', padding: 20 },
  title: { fontSize: 26, fontWeight: 'bold', marginBottom: 20, textAlign: 'center' },
  input: { height: 50, borderRadius: 8, paddingHorizontal: 10, marginBottom: 12 },
});
