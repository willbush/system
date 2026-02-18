return {
  {
    -- This handles setting lsp for rust
    'mrcjkb/rustaceanvim',
    version = '^6',
    lazy = false, -- This plugin is already lazy
    config = function()
      vim.g.rustaceanvim = {
        server = {
          -- TODO: need to look into large project perf more..
          -- - maybe try https://github.com/ethowitz/cargo-subspace
          -- - maybe try https://github.com/crisidev/bacon-ls
          default_settings = {
            ['rust-analyzer'] = {
              checkOnSave = true,
            },
          },
          on_attach = function(_, bufnr)
            local function clue(mode, lhs, desc)
              vim.keymap.set(mode, lhs, '<nop>', {
                buffer = bufnr,
                silent = true,
                desc = desc,
              })
            end

            local function map(mode, lhs, rhs, desc)
              vim.keymap.set(mode, lhs, rhs, {
                buffer = bufnr,
                silent = true,
                desc = desc,
              })
            end

            local ll = vim.g.maplocalleader or ','

            -- Flycheck
            clue('n', ll .. 'c', '+FlyCheck')

            map('n', ll .. 'cc', function()
              vim.cmd.RustLsp('flyCheck')
            end, 'Fly check run')
            map('n', ll .. 'cl', function()
              vim.cmd.RustLsp({ 'flyCheck', 'clear' })
            end, 'Fly check clear')
            map('n', ll .. 'cx', function()
              vim.cmd.RustLsp({ 'flyCheck', 'cancel' })
            end, 'Fly check cancel')

            -- Runnables
            clue('n', ll .. 'r', '+Runnables')

            map('n', ll .. 'rr', function()
              vim.cmd.RustLsp('runnables')
            end, 'Rust runnables')

            map('n', ll .. 'rt', function()
              vim.cmd.RustLsp('testables')
            end, 'Rust testables')

            -- Error helpers
            clue('n', ll .. 'e', '+Errors')

            map('n', ll .. 'ee', function()
              vim.cmd.RustLsp('explainError')
            end, 'Explain Rust error')

            map('n', ll .. 'er', function()
              vim.cmd.RustLsp('renderDiagnostic')
            end, 'Render diagnostic (cargo-style)')

            -- goto Docs / navigation
            clue('n', ll .. 'g', '+Goto')

            map('n', ll .. 'go', function()
              vim.cmd.RustLsp('openDocs')
            end, 'Open docs.rs for symbol')

            map('n', ll .. 'gc', function()
              vim.cmd.RustLsp('openCargo')
            end, 'Open Cargo.toml')

            map('n', ll .. 'gp', function()
              vim.cmd.RustLsp('parentModule')
            end, 'Parent module')

            -- Trees / graphs / views
            clue('n', ll .. 'v', '+View')

            map('n', ll .. 'vs', function()
              vim.cmd.RustLsp('syntaxTree')
            end, 'Show Rust syntax tree')

            map('n', ll .. 'vg', function()
              vim.cmd.RustLsp('crateGraph')
            end, 'Crate graph (dot)')

            map('n', ll .. 'vm', function()
              vim.cmd.RustLsp({ 'view', 'mir' })
            end, 'View MIR for item')

            -- Macro
            map('n', ll .. 'vx', function()
              vim.cmd.RustLsp('expandMacro')
            end, 'Expand macro')

            -- structural editing
            map('n', ll .. 'j', function()
              vim.cmd.RustLsp('joinLines')
            end, 'Rust-aware join lines')

            map({ 'n', 'x' }, ll .. '/', function()
              vim.cmd.RustLsp('ssr')
            end, 'Structural search & replace')
          end,
        },
      }
    end,
  },
}
