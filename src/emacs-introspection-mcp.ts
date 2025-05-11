import { FastMCP } from "fastmcp";
import { z } from "zod";
import { execSync } from "child_process";

const mcp = new FastMCP({
    name: "emacs-introspection",
    version: "1.0.0",
});

mcp.addTool({
    name: "describe_function",
    description: "Get documentation for an Emacs function",
    parameters: z.object({
        function_name: z.string(),
    }),
    execute: async (args) => {
        const cmd = `emacsclient -e "(documentation '${args.function_name})"`;
        return execSync(cmd, { encoding: "utf8" }).trim();
    },
});

mcp.addTool({
    name: "describe_variable",
    description: "Get documentation for an Emacs variable",
    parameters: z.object({
        variable_name: z.string(),
    }),
    execute: async (args) => {
        const cmd = `emacsclient -e "(documentation-property '${args.variable_name} 'variable-documentation)"`;
        return execSync(cmd, { encoding: "utf8" }).trim();
    },
});

mcp.addTool({
    name: "get_variable_value",
    description: "Get the current value of an Emacs variable",
    parameters: z.object({
        variable_name: z.string(),
    }),
    execute: async (args) => {
        const cmd = `emacsclient -e "${args.variable_name}"`;
        return execSync(cmd, { encoding: "utf8" }).trim();
    },
});

mcp.start({
    transportType: "stdio",
});
