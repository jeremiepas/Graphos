// Infrastructure: Logger service

export enum LogLevel {
  Debug = 0,
  Info = 1,
  Warn = 2,
  Error = 3,
}

export class Logger {
  private level: LogLevel;
  private prefix: string;

  constructor(prefix: string = "app", level: LogLevel = LogLevel.Info) {
    this.prefix = prefix;
    this.level = level;
  }

  debug(message: string, ...args: unknown[]): void {
    if (this.level <= LogLevel.Debug) {
      console.debug(`[${this.prefix}] DEBUG:`, message, ...args);
    }
  }

  info(message: string, ...args: unknown[]): void {
    if (this.level <= LogLevel.Info) {
      console.info(`[${this.prefix}] INFO:`, message, ...args);
    }
  }

  warn(message: string, ...args: unknown[]): void {
    if (this.level <= LogLevel.Warn) {
      console.warn(`[${this.prefix}] WARN:`, message, ...args);
    }
  }

  error(message: string, ...args: unknown[]): void {
    if (this.level <= LogLevel.Error) {
      console.error(`[${this.prefix}] ERROR:`, message, ...args);
    }
  }
}

export const logger = new Logger("ts-lsp-test");