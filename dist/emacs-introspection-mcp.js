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
const mcp = new fastmcp_1.FastMCP({
    name: "emacs-introspection",
    version: "1.0.0",
});
// TODO[A5OhAyxaiJ] Overly restrictive? Note sufficient sanitization?
const isValidEmacsSymbol = (str) => /^[a-zA-Z0-9-_]+$/.test(str);
mcp.addTool({
    name: "describe_function",
    description: "Get documentation for an Emacs function",
    parameters: zod_1.z.object({
        function_name: zod_1.z
            .string()
            .refine(isValidEmacsSymbol, "Invalid Emacs symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        const cmd = `emacsclient -e "(documentation '${args.function_name})"`;
        return (0, child_process_1.execSync)(cmd, { encoding: "utf8" }).trim();
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
        const cmd = `emacsclient -e "(documentation-property '${args.variable_name} 'variable-documentation)"`;
        return (0, child_process_1.execSync)(cmd, { encoding: "utf8" }).trim();
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
        const cmd = `emacsclient -e "${args.variable_name}"`;
        return (0, child_process_1.execSync)(cmd, { encoding: "utf8" }).trim();
    }),
});
mcp.start({
    transportType: "stdio",
});
