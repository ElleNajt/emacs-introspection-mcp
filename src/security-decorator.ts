/**
 * Decorator-style security wrapper for MCP tools
 * Automatically validates all string arguments for injection attacks
 */

import { validateNoInjection } from './security';

/**
 * Check all arguments recursively for dangerous patterns
 */
function checkAllArgs(obj: any, path: string = 'args'): void {
    if (typeof obj === 'string') {
        validateNoInjection(obj, path);
        return;
    }
    
    if (Array.isArray(obj)) {
        obj.forEach((item, index) => {
            checkAllArgs(item, `${path}[${index}]`);
        });
        return;
    }
    
    if (obj && typeof obj === 'object') {
        Object.entries(obj).forEach(([key, value]) => {
            checkAllArgs(value, `${path}.${key}`);
        });
        return;
    }
    
    // Numbers, booleans, null, undefined are safe
}

/**
 * Decorator that wraps any function to validate all string arguments
 */
export function secureArgs<T extends (...args: any[]) => any>(fn: T, toolName: string): T {
    return ((...args: Parameters<T>) => {
        try {
            // Check all arguments for dangerous patterns
            args.forEach((arg, index) => {
                checkAllArgs(arg, `${toolName}.arg${index}`);
            });
            
            // Call original function if validation passes
            return fn(...args);
        } catch (error: any) {
            // Re-throw with better error message
            throw new Error(`Security validation failed for ${toolName}: ${error.message}`);
        }
    }) as T;
}

/**
 * Helper to wrap MCP tool execute functions
 */
export function secureMCPTool(toolDefinition: any): any {
    const originalExecute = toolDefinition.execute;
    
    return {
        ...toolDefinition,
        execute: secureArgs(originalExecute, toolDefinition.name)
    };
}