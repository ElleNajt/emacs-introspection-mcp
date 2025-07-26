#!/usr/bin/env node
"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const fastmcp_1 = require("fastmcp");
const zod_1 = require("zod");
const child_process_1 = require("child_process");
const fs_1 = require("fs");
const path_1 = require("path");
const mcp = new fastmcp_1.FastMCP({
    name: "emacs-introspection",
    version: "1.0.0",
});
// TODO[A5OhAyxaiJ] Overly restrictive? Not sufficient sanitization?
const isValidEmacsSymbol = (str) => /^[a-zA-Z0-9-_]+$/.test(str);
mcp.addTool({
    name: "describe_function",
    description: "Get documentation for an Emacs function",
    parameters: zod_1.z.object({
        function_name: zod_1.z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    // Using execfile because it is safe from shell injection.
    // https://nodejs.org/api/child_process.html#child_processexecfilefile-args-options-callback
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            (0, child_process_1.execFile)("emacsclient", ["-e", `(documentation '${args.function_name})`], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "describe_variable",
    description: "Get documentation for an Emacs variable",
    parameters: zod_1.z.object({
        variable_name: zod_1.z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            (0, child_process_1.execFile)("emacsclient", [
                "-e",
                `(documentation-property '${args.variable_name} 'variable-documentation)`,
            ], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "get_variable_value",
    description: "Get the current value of an Emacs variable",
    parameters: zod_1.z.object({
        variable_name: zod_1.z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            (0, child_process_1.execFile)("emacsclient", ["-e", `(symbol-value '${args.variable_name})`], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                resolve(stdout.trim());
            });
        });
    }),
});
mcp.addTool({
    name: "view_buffer",
    description: "Get the contents of a specific Emacs buffer and write it to /tmp/ClaudeWorkingFolder/<buffer_name>.txt. Returns the file path so Claude can use other tools like Read, Grep, etc. to analyze the buffer content.",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().min(1, "Buffer name cannot be empty"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            (0, child_process_1.execFile)("emacsclient", ["-e", `(with-current-buffer "${args.buffer_name}" (buffer-string))`], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                const content = stdout.trim();
                try {
                    // Ensure /tmp/ClaudeWorkingFolder directory exists
                    (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
                    // Create filename from buffer name (sanitize special characters)
                    const sanitizedBufferName = args.buffer_name.replace(/[^a-zA-Z0-9-_]/g, "_");
                    const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `${sanitizedBufferName}.txt`);
                    (0, fs_1.writeFileSync)(filename, content, "utf8");
                    resolve(`Buffer content written to ${filename}`);
                }
                catch (writeError) {
                    reject(new Error(`Failed to write file: ${writeError}`));
                }
            });
        });
    }),
});
mcp.addTool({
    name: "get_agenda",
    description: "Get the org-agenda view and write it to /tmp/ClaudeWorkingFolder/agenda_<agenda_type>.txt. Returns the file path so Claude can use other tools like Read, Grep, etc. to analyze the agenda content.",
    parameters: zod_1.z.object({
        agenda_type: zod_1.z.string().optional().default("a"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            // Ensure /tmp/ClaudeWorkingFolder directory exists
            (0, fs_1.mkdirSync)("/tmp/ClaudeWorkingFolder", { recursive: true });
            // Create filename from agenda type
            const filename = (0, path_1.join)("/tmp/ClaudeWorkingFolder", `agenda_${args.agenda_type}.txt`);
            const absolutePath = filename;
            (0, child_process_1.execFile)("emacsclient", ["-e", `(save-window-excursion (let ((org-agenda-window-setup 'current-window)) (org-agenda nil "${args.agenda_type}") (with-current-buffer "*Org Agenda*" (write-file "${absolutePath}"))))`], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(`Agenda content written to ${filename}`);
                }
            });
        });
    }),
});
mcp.start({
    transportType: "stdio",
});
