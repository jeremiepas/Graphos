// Infrastructure: Database simulation and ID generation

import { User, UserId } from "../types";

let nextId: UserId = 1;

export function generateId(): UserId {
  return nextId++;
}

// In-memory user storage
const users: Map<UserId, User> = new Map();

export function dbGetAll(): User[] {
  return Array.from(users.values());
}

export function dbGetById(id: UserId): User | undefined {
  return users.get(id);
}

export function dbCreate(user: User): User {
  users.set(user.id, user);
  return user;
}

export function dbUpdate(id: UserId, updates: Partial<User>): User | undefined {
  const existing = users.get(id);
  if (!existing) return undefined;
  const updated = { ...existing, ...updates };
  users.set(id, updated);
  return updated;
}

export function dbDelete(id: UserId): boolean {
  return users.delete(id);
}