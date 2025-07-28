#!/usr/bin/env node
"use strict";
// Eglot LSP tools for Emacs introspection
// These tools are currently not working and need eglot to be properly configured
// Move these back to main file when eglot is working
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
const isValidBufferName = (str) => str.length > 0 && str.length < 256;
const isValidSymbolName = (str) => /^[a-zA-Z0-9-_:]+$/.test(str);
const mcp = new fastmcp_1.FastMCP({
    name: "emacs-eglot-tools",
    version: "1.0.0",
});
mcp.addTool({
    name: "eglot_help_at_point",
    description: "Get LSP hover information for a symbol at a specific line in a buffer",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().refine(isValidBufferName, "Invalid buffer name"),
        line_number: zod_1.z.number().int().positive(),
        symbol_name: zod_1.z.string().refine(isValidSymbolName, "Invalid symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(with-current-buffer "${args.buffer_name}" (save-excursion (goto-char (point-min)) (forward-line ${args.line_number - 1}) (when (search-forward "${args.symbol_name}" (line-end-position) t) (goto-char (match-beginning 0)) (if (fboundp 'eglot-help-at-point) (eglot-help-at-point) "eglot-help-at-point not available"))))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim());
                }
            });
        });
    }),
});
mcp.addTool({
    name: "eglot_find_definition",
    description: "Find definition of a symbol at a specific line in a buffer",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().refine(isValidBufferName, "Invalid buffer name"),
        line_number: zod_1.z.number().int().positive(),
        symbol_name: zod_1.z.string().refine(isValidSymbolName, "Invalid symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(with-current-buffer "${args.buffer_name}" (save-excursion (goto-char (point-min)) (forward-line ${args.line_number - 1}) (when (search-forward "${args.symbol_name}" (line-end-position) t) (goto-char (match-beginning 0)) (if (fboundp 'eglot-find-definition) (progn (eglot-find-definition) (format "Jumped to %s:%d" (buffer-name) (line-number-at-pos))) "eglot-find-definition not available"))))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim());
                }
            });
        });
    }),
});
mcp.addTool({
    name: "eglot_find_references",
    description: "Find references to a symbol at a specific line in a buffer",
    parameters: zod_1.z.object({
        buffer_name: zod_1.z.string().refine(isValidBufferName, "Invalid buffer name"),
        line_number: zod_1.z.number().int().positive(),
        symbol_name: zod_1.z.string().refine(isValidSymbolName, "Invalid symbol name"),
    }),
    execute: (args) => __awaiter(void 0, void 0, void 0, function* () {
        return new Promise((resolve, reject) => {
            const elisp = `(with-current-buffer "${args.buffer_name}" (save-excursion (goto-char (point-min)) (forward-line ${args.line_number - 1}) (when (search-forward "${args.symbol_name}" (line-end-position) t) (goto-char (match-beginning 0)) (if (fboundp 'eglot-find-references) (progn (eglot-find-references) "References found - check *xref* buffer") "eglot-find-references not available"))))`;
            (0, child_process_1.execFile)("emacsclient", ["-e", elisp], { encoding: "utf8", shell: false }, (error, stdout, stderr) => {
                if (error) {
                    reject(error);
                }
                else {
                    resolve(stdout.trim());
                }
            });
        });
    }),
});
mcp.start({
    transportType: "stdio",
});
