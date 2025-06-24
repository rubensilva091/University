import React, { useState, useEffect } from 'react';
import { View, Text, StyleSheet, Button } from 'react-native';
import { Ionicons } from '@expo/vector-icons';
import { useCompassRotation } from './bussola';

// Helper function to convert degrees to radians
function deg2rad(deg: number): number {
    return deg * (Math.PI / 180);
}

// Function to calculate distance between two points
function calculateDistance(lat1: number, lon1: number, lat2: number, lon2: number): number {
    const R = 6371; // Earth's radius in kilometers
    const dLat = deg2rad(lat2 - lat1); // Difference in latitude
    const dLon = deg2rad(lon2 - lon1); // Difference in longitude
    const a =
        Math.sin(dLat / 2) * Math.sin(dLat / 2) +
        Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    const distanceInKm = R * c; // Distance in kilometers
    return distanceInKm * 1000; // Convert to meters
}

// Function to calculate bearing between two points
function calculateBearing(startLat: number, startLng: number, destLat: number, destLng: number): number {
    const startLatRad = deg2rad(startLat);
    const startLngRad = deg2rad(startLng);
    const destLatRad = deg2rad(destLat);
    const destLngRad = deg2rad(destLng);
    const y = Math.sin(destLngRad - startLngRad) * Math.cos(destLatRad);
    const x = Math.cos(startLatRad) * Math.sin(destLatRad) - Math.sin(startLatRad) * Math.cos(destLatRad) * Math.cos(destLngRad - startLngRad);
    const bearing = Math.atan2(y, x);
    return (bearing * 180 / Math.PI + 360) % 360;
}

interface FinderProps {
    currentLocation: { latitude: number; longitude: number };
    destination: { latitude: number; longitude: number; name: string };
    fullScreen?: boolean;
    onClose?: () => void;
}

export const Finder: React.FC<FinderProps> = ({
    currentLocation,
    destination,
    fullScreen = false,
    onClose,
}) => {
    const [arrowAngle, setArrowAngle] = useState(0);
    const { heading } = useCompassRotation();
    const HEADING_OFFSET = 45;
    const deviceHeading = (heading - HEADING_OFFSET + 360) % 360;

    useEffect(() => {
        const bearing = calculateBearing(
            currentLocation.latitude,
            currentLocation.longitude,
            destination.latitude,
            destination.longitude
        );
        const angle = (bearing - deviceHeading + 360) % 360;
        setArrowAngle(angle);
    }, [currentLocation, destination, deviceHeading]);

    const distance = calculateDistance(
        currentLocation.latitude,
        currentLocation.longitude,
        destination.latitude,
        destination.longitude
    );

    const containerStyle = fullScreen
        ? [styles.container, styles.fullScreen]
        : styles.container;

    return (
        <View style={containerStyle}>
            {fullScreen && onClose && (
                <View style={styles.closeButton}>
                    <Button title="Fechar" onPress={onClose} />
                </View>
            )}
            <Text style={styles.title}>
                {destination.name} - {distance.toFixed(0)} m
            </Text>
            <Ionicons
                name="arrow-up"
                size={100}
                color={fullScreen ? 'white' : 'black'}
                style={{ transform: [{ rotate: `${arrowAngle}deg` }] }}
            />
        </View>
    );
};

const styles = StyleSheet.create({
    container: {
        position: 'absolute',
        bottom: 100,
        alignSelf: 'center',
        alignItems: 'center',
        zIndex: 10,
    },
    fullScreen: {
        position: 'absolute',
        top: 0,
        left: 0,
        right: 0,
        bottom: 0,
        backgroundColor: 'black',
        justifyContent: 'center',
        alignItems: 'center',
    },
    title: {
        fontSize: 24,
        fontWeight: 'bold',
        color: 'white',
        marginBottom: 20,
    },
    closeButton: {
        position: 'absolute',
        top: 40,
        right: 20,
    },
});