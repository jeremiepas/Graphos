// UseCase: User service — business logic orchestrating DB and validation

import { User, UserId, UserRole, CreateUserRequest, UpdateUserRequest } from "./types";
import { generateId, dbGetAll, dbGetById, dbCreate, dbUpdate, dbDelete } from "./db";
import { validateCreateRequest, validateUpdateRequest } from "./validate";
import { logger } from "./utils/logger";

export class UserService {
  getAllUsers(): User[] {
    logger.info("Fetching all users");
    return dbGetAll();
  }

  getUserById(id: UserId): User | null {
    const user = dbGetById(id);
    if (!user) {
      logger.warn(`User not found: ${id}`);
      return null;
    }
    return user;
  }

  createUser(req: CreateUserRequest): User | null {
    const validation = validateCreateRequest(req);
    if (!validation.valid) {
      logger.error(`Validation failed: ${validation.errors.join(", ")}`);
      return null;
    }

    const user: User = {
      id: generateId(),
      name: req.name,
      email: req.email,
      role: req.role,
      createdAt: new Date(),
    };

    logger.info(`Creating user: ${user.name}`);
    return dbCreate(user);
  }

  updateUser(id: UserId, req: UpdateUserRequest): User | null {
    const validation = validateUpdateRequest(req);
    if (!validation.valid) {
      logger.error(`Validation failed: ${validation.errors.join(", ")}`);
      return null;
    }

    const updated = dbUpdate(id, req);
    if (!updated) {
      logger.warn(`User not found for update: ${id}`);
      return null;
    }

    logger.info(`Updated user: ${updated.name}`);
    return updated;
  }

  deleteUser(id: UserId): boolean {
    const deleted = dbDelete(id);
    if (!deleted) {
      logger.warn(`User not found for deletion: ${id}`);
    }
    return deleted;
  }

  getAdmins(): User[] {
    return dbGetAll().filter((user) => user.role === UserRole.Admin);
  }
}

// Singleton instance
export const userService = new UserService();