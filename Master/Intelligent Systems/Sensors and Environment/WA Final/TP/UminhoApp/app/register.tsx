import React, { useState } from 'react';
import { View, TextInput, Button, StyleSheet, Text, Alert, useColorScheme } from 'react-native';
import { createUserWithEmailAndPassword } from 'firebase/auth';
import { auth } from '../constants/firebaseConfig';
import { useRouter } from 'expo-router';

export default function RegisterScreen() {
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const router = useRouter();
  const colorScheme = useColorScheme();

  const handleRegister = async () => {
    try {
      await createUserWithEmailAndPassword(auth, email, password);
      Alert.alert('Conta criada com sucesso!');
      router.replace('/(tabs)/'); // Redireciona depois de registar
    } catch (error: any) {
      Alert.alert('Erro', error.message);
    }
  };

  return (
    <View style={[styles.container, { backgroundColor: colorScheme === 'dark' ? '#000' : '#fff' }]}>
      <Text style={[styles.title, { color: colorScheme === 'dark' ? '#fff' : '#000' }]}>Registar</Text>

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

      <Button title="Criar Conta" onPress={handleRegister} />
      <View style={{ height: 10 }} />
      <Button title="Já tens conta? Faz login" onPress={() => router.push('/login')} />
    </View>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, justifyContent: 'center', padding: 20 },
  title: { fontSize: 26, fontWeight: 'bold', marginBottom: 20, textAlign: 'center' },
  input: { height: 50, borderRadius: 8, paddingHorizontal: 10, marginBottom: 12 },
});
