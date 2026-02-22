return {
  {
    'lewis6991/gitsigns.nvim',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns

        local map = function(mode, lhs, rhs, desc)
          vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
        end

        -- Navigation
        map('n', ']h', function()
          if vim.wo.diff then
            vim.cmd.normal { ']c', bang = true }
          else
            gs.nav_hunk('next', { target = 'all' })
          end
        end, 'Next hunk')

        map('n', '[h', function()
          if vim.wo.diff then
            vim.cmd.normal { '[c', bang = true }
          else
            gs.nav_hunk('prev', { target = 'all' })
          end
        end, 'Previous hunk')

        -- Actions
        map('n', '<leader>hs', gs.stage_hunk, 'Stage hunk')
        map('n', '<leader>hr', gs.reset_hunk, 'Reset hunk')
        map('n', '<leader>hu', gs.undo_stage_hunk, 'Undo stage hunk')

        map('n', '<leader>hS', gs.stage_buffer, 'Stage buffer')
        map('n', '<leader>hR', gs.reset_buffer, 'Reset buffer')

        map('n', '<leader>hp', gs.preview_hunk, 'Preview hunk')
        map('n', '<leader>hi', gs.preview_hunk_inline, 'Preview hunk inline')

        map('n', '<leader>hb', function()
          gs.blame_line { full = true }
        end, 'Blame line')

        map('n', '<leader>hd', gs.diffthis, 'Diff this')
        map('n', '<leader>hD', function()
          gs.diffthis '~'
        end, 'Diff this ~')

        map('n', '<leader>hq', gs.setqflist, 'Hunks to quickfix')
        map('n', '<leader>hQ', function()
          gs.setqflist 'all'
        end, 'Hunks to quickfix (all)')

        -- Toggles
        map('n', '<leader>tb', gs.toggle_current_line_blame, 'Toggle line blame')
        map('n', '<leader>tw', gs.toggle_word_diff, 'Toggle word diff')

        -- Text object
        -- note using k due to my remappings: i ("inner") -> k ("kernel").
        map({ 'o', 'x' }, 'kh', gs.select_hunk, 'Select hunk')
      end,
    },
  },
}
