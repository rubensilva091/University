import React, { useEffect, useState } from 'react';
import { View, Text, StyleSheet, ActivityIndicator, ScrollView } from 'react-native';
import { getUserBadges } from '../constants/firebaseHelpers';
import { allBadges } from '../constants/badges';

export default function BadgesScreen() {
  const [userBadges, setUserBadges] = useState<{ [key: string]: boolean }>({});
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchBadges = async () => {
      const badges = await getUserBadges();
      setUserBadges(badges);
      setLoading(false);
    };

    fetchBadges();
  }, []);

  if (loading) {
    return (
      <View style={styles.center}>
        <ActivityIndicator size="large" />
        <Text>Carregando conquistas...</Text>
      </View>
    );
  }

  return (
    <ScrollView contentContainerStyle={styles.container}>
      <Text style={styles.title}>As Tuas Conquistas</Text>
      {allBadges.map((badge) => {
        const unlocked = userBadges[badge.id];

        return (
          <View key={badge.id} style={[styles.badge, { opacity: unlocked ? 1 : 0.3 }]}>
            <Text style={styles.icon}>{badge.icon}</Text>
            <View>
              <Text style={styles.badgeTitle}>{badge.title}</Text>
              <Text style={styles.badgeDesc}>{badge.description}</Text>
              {!unlocked && <Text style={styles.locked}>🚫 Ainda não conquistado</Text>}
            </View>
          </View>
        );
      })}
    </ScrollView>
  );
}

const styles = StyleSheet.create({
  container: {
    padding: 20,
    paddingBottom: 100,
  },
  center: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  title: {
    fontSize: 26,
    fontWeight: 'bold',
    marginBottom: 20,
    textAlign: 'center',
  },
  badge: {
    flexDirection: 'row',
    alignItems: 'center',
    backgroundColor: '#eee',
    marginBottom: 15,
    padding: 15,
    borderRadius: 12,
  },
  icon: {
    fontSize: 32,
    marginRight: 15,
  },
  badgeTitle: {
    fontSize: 18,
    fontWeight: 'bold',
  },
  badgeDesc: {
    fontSize: 14,
    color: '#555',
  },
  locked: {
    marginTop: 4,
    fontSize: 12,
    color: 'red',
  },
});