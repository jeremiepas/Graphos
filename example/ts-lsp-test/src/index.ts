// Main entry point — demonstrates all exports and cross-module references

import { UserService, userService } from "./service";
import { User, UserRole } from "./types";
import { formatUserName } from "./utils/format";
import { Logger, LogLevel } from "./utils/logger";

const appLogger = new Logger("main", LogLevel.Debug);

function printUser(user: User): void {
  appLogger.info(`User: ${formatUserName(user)} (${user.role})`);
}

function main(): void {
  // Create some users
  const alice = userService.createUser({
    name: "Alice",
    email: "alice@example.com",
    role: UserRole.Admin,
  });

  const bob = userService.createUser({
    name: "Bob",
    email: "bob@example.com",
    role: UserRole.Editor,
  });

  const charlie = userService.createUser({
    name: "Charlie",
    email: "charlie@example.com",
    role: UserRole.Viewer,
  });

  // Print all users
  const allUsers = userService.getAllUsers();
  allUsers.forEach(printUser);

  // Find admins
  const admins = userService.getAdmins();
  appLogger.info(`Admins: ${admins.length}`);

  // Update a user
  if (bob) {
    userService.updateUser(bob.id, { role: UserRole.Admin });
  }

  // Delete a user
  if (charlie) {
    userService.deleteUser(charlie.id);
  }

  appLogger.info("Done");
}

main();