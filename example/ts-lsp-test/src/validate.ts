// UseCase: Validation logic for user operations

import { UserRole, CreateUserRequest, UpdateUserRequest } from "../types";

export interface ValidationResult {
  valid: boolean;
  errors: string[];
}

export function validateCreateRequest(req: CreateUserRequest): ValidationResult {
  const errors: string[] = [];

  if (!req.name || req.name.trim().length < 2) {
    errors.push("Name must be at least 2 characters");
  }

  if (!req.email || !req.email.includes("@")) {
    errors.push("Valid email is required");
  }

  if (!Object.values(UserRole).includes(req.role)) {
    errors.push("Invalid role");
  }

  return { valid: errors.length === 0, errors };
}

export function validateUpdateRequest(req: UpdateUserRequest): ValidationResult {
  const errors: string[] = [];

  if (req.name !== undefined && req.name.trim().length < 2) {
    errors.push("Name must be at least 2 characters");
  }

  if (req.email !== undefined && !req.email.includes("@")) {
    errors.push("Valid email is required");
  }

  if (req.role !== undefined && !Object.values(UserRole).includes(req.role)) {
    errors.push("Invalid role");
  }

  return { valid: errors.length === 0, errors };
}