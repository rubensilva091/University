import { doc, getDoc, updateDoc, setDoc } from 'firebase/firestore';
import { auth, db } from './firebaseConfig';

export const addVisitedBuilding = async (buildingId: string) => {
  const user = auth.currentUser;
  if (!user) return;

  const userRef = doc(db, 'users', user.uid);
  const docSnap = await getDoc(userRef);

  if (docSnap.exists()) {
    const data = docSnap.data();
    const visited = data.visitedBuildings || [];

    if (!visited.includes(buildingId)) {
      await updateDoc(userRef, {
        visitedBuildings: [...visited, buildingId],
      });
    }
  } else {
    await setDoc(userRef, {
      visitedBuildings: [buildingId],
    });
  }
};

export const getVisitedBuildings = async (): Promise<string[]> => {
  const user = auth.currentUser;
  if (!user) return [];

  const userRef = doc(db, 'users', user.uid);
  const docSnap = await getDoc(userRef);

  if (docSnap.exists()) {
    return docSnap.data()?.visitedBuildings || [];
  }

  return [];
};

export const getUserBadges = async (): Promise<Record<string, boolean>> => {
    const user = auth.currentUser;
    if (!user) {
      console.warn("Nenhum utilizador autenticado.");
      return {};
    }
  
    try {
      const userRef = doc(db, 'users', user.uid);
      const docSnap = await getDoc(userRef);
  
      if (docSnap.exists()) {
        const data = docSnap.data();
        return data?.badges || {}; // ← protege se badges ainda não existir
      } else {
        console.log("Documento do utilizador ainda não existe.");
        return {};
      }
    } catch (error) {
      console.error("Erro ao obter badges:", error);
      return {};
    }
  };

  export const saveBadge = async (badgeId: string) => {
    const user = auth.currentUser;
    if (!user) return;
  
    const userRef = doc(db, 'users', user.uid);
  
    // Atualiza o badge no Firestore
    await updateDoc(userRef, {
      [`badges.${badgeId}`]: true, 
    }).catch(async (error) => {
      console.warn("⚠️ updateDoc falhou, tentando criar doc:", error);
      // Se falhar, cria o documento com o badge
      await setDoc(userRef, { badges: { [badgeId]: true } }, { merge: true });
    });
  };