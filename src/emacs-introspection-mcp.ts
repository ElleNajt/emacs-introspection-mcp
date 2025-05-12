#!/usr/bin/env node

import { FastMCP } from "fastmcp";
import { z } from "zod";
import { execFile } from "child_process";

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

mcp.start({
    transportType: "stdio",
});
