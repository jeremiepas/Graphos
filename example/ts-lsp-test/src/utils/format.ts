// Domain: Formatting helpers

import { User } from "../types";

export function formatUserName(user: User): string {
  return `${user.name} <${user.email}>`;
}

export function formatUserList(users: User[]): string {
  return users.map(formatUserName).join(", ");
}