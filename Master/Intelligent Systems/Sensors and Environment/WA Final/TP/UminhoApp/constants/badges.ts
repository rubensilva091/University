import badgeData from '../assets/badges.json';

export type Badge = {
  id: string;
  title: string;
  description: string;
  icon: string;
  required: number;
  condition: (context: { visitedBuildings: string[] }) => boolean;
};

export const allBadges: Badge[] = badgeData.map((b) => ({
  ...b,

  condition: ({ visitedBuildings }) => visitedBuildings.length >= b.required,
}));