import tseslint from 'typescript-eslint';

export default tseslint.config(
  {
    files: ['client/src/**/*.ts', 'server/src/**/*.ts'],
    extends: [...tseslint.configs.recommended],
    rules: {
      '@typescript-eslint/no-explicit-any': 'warn',
      '@typescript-eslint/no-unused-vars': [
        'error',
        { argsIgnorePattern: '^_', varsIgnorePattern: '^_' },
      ],
    },
  },
);
