/**
 * Security utilities for Emacs introspection MCP server
 * Provides input sanitization and validation to prevent injection attacks
 */

export class SecurityError extends Error {
    constructor(message: string, public readonly input?: string) {
        super(message);
        this.name = 'SecurityError';
    }
}

/**
 * Dangerous patterns that indicate potential injection attempts
 */
const DANGEROUS_PATTERNS = [
    /"\s*\)\s*;\s*\(/,           // "); (
    /'\s*\)\s*;\s*\(/,           // '); (
    /\\"\s*\)\s*;\s*\(/,         // \"); (
    /\\'\s*\)\s*;\s*\(/,         // \'); (
    /(shell-command|call-process|eval|load-file|write-region)/i,
    /(delete-file|delete-directory)/i,
    /(setq|defun|defvar|defcustom)/i,
    /^\s*[\(\)]/,                // Starts with parens (potential elisp)
];

/**
 * Validate that a string doesn't contain injection patterns
 */
export function validateNoInjection(input: string, context: string = 'input'): void {
    if (typeof input !== 'string' || input === '' || input == null) {
        throw new SecurityError(`Invalid ${context}: must be a non-empty string`, input);
    }

    for (const pattern of DANGEROUS_PATTERNS) {
        if (pattern.test(input)) {
            throw new SecurityError(
                `Potentially dangerous pattern detected in ${context}: ${pattern}`, 
                input
            );
        }
    }

    // Check for excessive length (potential DoS)
    if (input.length > 1000) {
        throw new SecurityError(`${context} too long (max 1000 characters)`, input);
    }
}

/**
 * Escape a string for safe use in Elisp code
 */
export function escapeElispString(str: string): string {
    validateNoInjection(str, 'elisp string');
    
    return str
        .replace(/\\/g, '\\\\')     // Escape backslashes first
        .replace(/"/g, '\\"')       // Escape double quotes
        .replace(/\n/g, '\\n')      // Escape newlines
        .replace(/\r/g, '\\r')      // Escape carriage returns
        .replace(/\t/g, '\\t');     // Escape tabs
}

/**
 * Validate Emacs symbol names more strictly
 */
export function validateEmacsSymbol(symbol: string): void {
    if (!symbol || typeof symbol !== 'string') {
        throw new SecurityError('Symbol must be a non-empty string', symbol);
    }

    if (symbol.length > 100) {
        throw new SecurityError('Symbol name too long (max 100 characters)', symbol);
    }

    // Allow only safe characters for Emacs symbols
    if (!/^[a-zA-Z][a-zA-Z0-9\-_:+*/?<>=!]*$/.test(symbol)) {
        throw new SecurityError('Invalid symbol name format', symbol);
    }

    // Skip validateNoInjection for symbols since they have stricter validation
}

/**
 * Validate buffer names
 */
export function validateBufferName(bufferName: string): void {
    if (!bufferName || typeof bufferName !== 'string') {
        throw new SecurityError('Buffer name must be a non-empty string', bufferName);
    }

    if (bufferName.length > 256) {
        throw new SecurityError('Buffer name too long (max 256 characters)', bufferName);
    }

    validateNoInjection(bufferName, 'buffer name');
}

/**
 * Validate search patterns more strictly
 */
export function validateSearchPattern(pattern: string): void {
    if (!pattern || typeof pattern !== 'string') {
        throw new SecurityError('Search pattern must be a non-empty string', pattern);
    }

    if (pattern.length > 200) {
        throw new SecurityError('Search pattern too long (max 200 characters)', pattern);
    }

    validateNoInjection(pattern, 'search pattern');
}

/**
 * Validate org content (headings, capture content, etc.)
 */
export function validateOrgContent(content: string): void {
    if (!content || typeof content !== 'string') {
        throw new SecurityError('Org content must be a non-empty string', content);
    }

    if (content.length > 5000) {
        throw new SecurityError('Org content too long (max 5000 characters)', content);
    }

    validateNoInjection(content, 'org content');
}

/**
 * Create a safe Elisp string literal
 */
export function createSafeElispLiteral(value: string): string {
    return `"${escapeElispString(value)}"`;
}

/**
 * Create a safe Elisp symbol reference
 */
export function createSafeElispSymbol(symbolName: string): string {
    validateEmacsSymbol(symbolName);
    return `'${symbolName}`;
}