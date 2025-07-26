#!/usr/bin/env node

import { FastMCP } from "fastmcp";
import { z } from "zod";
import { execFile } from "child_process";
import { writeFileSync, mkdirSync } from "fs";
import { join } from "path";

const mcp = new FastMCP({
    name: "emacs-introspection",
    version: "1.0.0",
});

// TODO[A5OhAyxaiJ] Overly restrictive? Not sufficient sanitization?
const isValidEmacsSymbol = (str: string) => /^[a-zA-Z0-9-_]+$/.test(str);

mcp.addTool({
    name: "describe_function",
    description: "Get documentation for an Emacs function",
    parameters: z.object({
        function_name: z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),

    // Using execfile because it is safe from shell injection.
    // https://nodejs.org/api/child_process.html#child_processexecfilefile-args-options-callback
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            execFile(
                "emacsclient",
                ["-e", `(documentation '${args.function_name})`],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    resolve(stdout.trim());
                },
            );
        });
    },
});

mcp.addTool({
    name: "describe_variable",
    description: "Get documentation for an Emacs variable",
    parameters: z.object({
        variable_name: z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            execFile(
                "emacsclient",
                [
                    "-e",
                    `(documentation-property '${args.variable_name} 'variable-documentation)`,
                ],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    resolve(stdout.trim());
                },
            );
        });
    },
});

mcp.addTool({
    name: "get_variable_value",
    description: "Get the current value of an Emacs variable",
    parameters: z.object({
        variable_name: z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            execFile(
                "emacsclient",
                ["-e", `(symbol-value '${args.variable_name})`],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    resolve(stdout.trim());
                },
            );
        });
    },
});

mcp.addTool({
    name: "view_buffer",
    description: "Get the contents of a specific Emacs buffer and write it to /tmp/ClaudeWorkingFolder/<buffer_name>.txt. Returns the file path so Claude can use other tools like Read, Grep, etc. to analyze the buffer content.",
    parameters: z.object({
        buffer_name: z.string().min(1, "Buffer name cannot be empty"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            execFile(
                "emacsclient",
                ["-e", `(with-current-buffer "${args.buffer_name}" (buffer-string))`],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    }
                    
                    const content = stdout.trim();
                    
                    try {
                        // Ensure /tmp/ClaudeWorkingFolder directory exists
                        mkdirSync("/tmp/ClaudeWorkingFolder", { recursive: true });
                        
                        // Create filename from buffer name (sanitize special characters)
                        const sanitizedBufferName = args.buffer_name.replace(/[^a-zA-Z0-9-_]/g, "_");
                        const filename = join("/tmp/ClaudeWorkingFolder", `${sanitizedBufferName}.txt`);
                        
                        writeFileSync(filename, content, "utf8");
                        resolve(`Buffer content written to ${filename}`);
                    } catch (writeError) {
                        reject(new Error(`Failed to write file: ${writeError}`));
                    }
                },
            );
        });
    },
});

mcp.addTool({
    name: "get_agenda",
    description: "Get the org-agenda view and write it to /tmp/ClaudeWorkingFolder/agenda_<agenda_type>.txt. Returns the file path so Claude can use other tools like Read, Grep, etc. to analyze the agenda content.",
    parameters: z.object({
        agenda_type: z.string().optional().default("a"),
    }),
    execute: async (args) => {
        return new Promise((resolve, reject) => {
            // Ensure /tmp/ClaudeWorkingFolder directory exists
            mkdirSync("/tmp/ClaudeWorkingFolder", { recursive: true });
            
            // Create filename from agenda type
            const filename = join("/tmp/ClaudeWorkingFolder", `agenda_${args.agenda_type}.txt`);
            const absolutePath = filename;
            
            execFile(
                "emacsclient",
                ["-e", `(save-window-excursion (let ((org-agenda-window-setup 'current-window)) (org-agenda nil "${args.agenda_type}") (with-current-buffer "*Org Agenda*" (write-file "${absolutePath}"))))`],
                { encoding: "utf8", shell: false },
                (error, stdout, stderr) => {
                    if (error) {
                        reject(error);
                    } else {
                        resolve(`Agenda content written to ${filename}`);
                    }
                },
            );
        });
    },
});

mcp.start({
    transportType: "stdio",
});
