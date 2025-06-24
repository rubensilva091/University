import { useState, useEffect } from 'react';
import { Image, StyleSheet, Alert } from 'react-native';
import { Magnetometer, Accelerometer } from 'expo-sensors';

// Props for the CompassIcon component
interface CompassIconProps {
  source: any; // The image source (e.g., tricornio.png)
}

// Custom hook to calculate smoothed magnetic heading
export const useCompassRotation = () => {
  const [smoothedHeading, setSmoothedHeading] = useState(0);
  const [magnetometerData, setMagnetometerData] = useState({ x: 0, y: 0, z: 0 });
  const [accelerometerData, setAccelerometerData] = useState({ x: 0, y: 0, z: 0 });
  const [isAvailable, setIsAvailable] = useState(true);

  useEffect(() => {
    // Check sensor availability
    const checkSensors = async () => {
      const magnetometerAvailable = await Magnetometer.isAvailableAsync();
      const accelerometerAvailable = await Accelerometer.isAvailableAsync();
      if (!magnetometerAvailable || !accelerometerAvailable) {
        setIsAvailable(false);
        Alert.alert(
          'Sensor Unavailable',
          'Magnetometer or Accelerometer not available. Compass functionality may be limited.',
        );
      }
    };
    checkSensors();

    // Set update intervals for sensors (100ms for smooth updates)
    Magnetometer.setUpdateInterval(100);
    Accelerometer.setUpdateInterval(100);

    // Subscribe to sensor updates
    const magnetometerSubscription = Magnetometer.addListener(setMagnetometerData);
    const accelerometerSubscription = Accelerometer.addListener(setAccelerometerData);

    // Clean up subscriptions when the component unmounts
    return () => {
      magnetometerSubscription.remove();
      accelerometerSubscription.remove();
    };
  }, []);

  useEffect(() => {
    if (!isAvailable) return;

    const { x: magX, y: magY, z: magZ } = magnetometerData;
    const { x: accX, y: accY, z: accZ } = accelerometerData;

    // Calculate pitch and roll from accelerometer data
    const pitch = Math.atan2(accY, Math.sqrt(accX * accX + accZ * accZ));
    const roll = Math.atan2(-accX, accZ);

    // Adjust magnetometer readings for tilt compensation
    const x = magX * Math.cos(pitch) + magZ * Math.sin(pitch);
    const y = magX * Math.sin(roll) * Math.sin(pitch) + magY * Math.cos(roll) - magZ * Math.sin(roll) * Math.cos(pitch);

    // Calculate magnetic heading in degrees
    let heading = Math.atan2(y, x) * (180 / Math.PI);
    if (heading < 0) heading += 360;

    // Adjust heading to correct for device orientation
    const adjustedHeading = (heading - 90 + 360) % 360;

    // Apply smoothing with proper angle wrap-around handling
    const alpha = 0.1; // Smoothing factor
    setSmoothedHeading(prev => {
      let diff = ((adjustedHeading - prev + 180) % 360 + 360) % 360 - 180;
      return (prev + alpha * diff + 360) % 360;
    });
  }, [magnetometerData, accelerometerData, isAvailable]);

  return { heading: smoothedHeading, isAvailable };
};

// Component to display the icon pointing to true north
export const CompassIcon: React.FC<CompassIconProps> = ({ source }) => {
  const { heading: magneticHeading, isAvailable } = useCompassRotation();

  // Base declination for May 8, 2025, at specified location
  const baseDeclination = -0.98; // 0.98° west
  const baseDate = new Date('2025-05-08');
  const annualChange = 0.16; // 0.16° east per year

  // Calculate current declination based on date
  const currentDate = new Date();
  const yearsElapsed = (currentDate.getTime() - baseDate.getTime()) / (1000 * 60 * 60 * 24 * 365.25);
  const declination = baseDeclination + annualChange * yearsElapsed;

  // Adjust for true north using declination
  const trueHeading = isAvailable ? (magneticHeading + declination + 360) % 360 : 0;
  // Rotate clockwise to match device turn direction
  const rotation = isAvailable ? trueHeading : 0;

  return (
    <Image
      source={source}
      style={[
        styles.icon,
        { transform: [{ rotate: `${rotation}deg` }] },
      ]}
    />
  );
};

// Styles for the icon
const styles = StyleSheet.create({
  icon: {
    width: 40,
    height: 40,
  },
});