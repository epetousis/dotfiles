-- vim: set tabstop=2:softtabstop=2:shiftwidth=2:expandtab:smarttab
local Plug = vim.fn['plug#']

vim.call('plug#begin')
  Plug 'vimwiki/vimwiki'
  Plug 'neovim/nvim-lspconfig'

  Plug('ms-jpq/coq_nvim', {['branch'] = 'coq'})
  Plug('ms-jpq/coq.artifacts', {['branch'] = 'artifacts'})

  Plug('junegunn/fzf', { ['do'] = vim.fn['fzf#install()'] })
  Plug 'junegunn/fzf.vim'

  Plug('glacambre/firenvim', { ['do'] = vim.fn['firenvim#install(0)'] })
  Plug 'folke/which-key.nvim'
  Plug 'justinmk/vim-sneak'
  Plug 'lervag/vimtex'
  Plug 'numToStr/Comment.nvim'

  -- treesitter: faster/better syntax highlighting
  Plug('nvim-treesitter/nvim-treesitter', { ['do'] = vim.fn['TSUpdate'] })
vim.call('plug#end')

-- Set up comment.nvim bindings
require('Comment').setup()

-- Label mode for vim-sneak
vim.g["sneak#label"] = 1

-- Treesitter setup
require'nvim-treesitter.configs'.setup {
  -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = "maintained",

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- List of parsers to ignore installing
  -- ignore_install = { "javascript" },

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- list of language that will be disabled
    -- disable = { "c", "rust" },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },

  indent = {
    enable = true
  },
}

-- Treesitter based folding
vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = vim.fn['nvim_treesitter#foldexpr']()

-- Language servers
vim.g.coq_settings = { ['auto_start'] = 'shut-up' }
local coq = require "coq"
require'lspconfig'.tsserver.setup(coq.lsp_ensure_capabilities{
  handlers = {
    ['window/showMessageRequest'] = function(_, result, params) return result end
  }
})
require'lspconfig'.eslint.setup(coq.lsp_ensure_capabilities{})
require'lspconfig'.pyright.setup(coq.lsp_ensure_capabilities{})
require'lspconfig'.vuels.setup(coq.lsp_ensure_capabilities{})
require'lspconfig'.rust_analyzer.setup(coq.lsp_ensure_capabilities{})

-- MAPPINGS
-- open FZF
vim.api.nvim_set_keymap('n', ',z', '<Cmd>FZF<CR>', {})
-- goto next buffer
vim.api.nvim_set_keymap('n', '<C-J>', ':bnext<CR>', {})
-- goto prev buffer
vim.api.nvim_set_keymap('n', '<C-K>', ':bprev<CR>', {})
-- yank to clipboard
vim.api.nvim_set_keymap('n', 'gy', '"+y', {})
-- paste from clipboard
vim.api.nvim_set_keymap('n', 'gp', '"+p', {})
-- set leader key to space
vim.g.mapleader = " "
-- BEGIN lspconfig bindings
local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
vim.api.nvim_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gh', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { 'pyright', 'rust_analyzer', 'tsserver', 'vuels' }
for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    flags = {
      -- This will be the default in neovim 0.7+
      debounce_text_changes = 150,
    }
  }
end
-- END lspconfig bindings

vim.opt.timeoutlen = 500

require("which-key").setup {
}

vim.opt.splitbelow = true -- open window splits below

vim.opt.laststatus = 2 -- enable statusline
vim.opt.statusline = "%f %m %r %l,%c %= %p%%" -- filename, modified flag, readonly flag, line/column number, file percentage on right

vim.g.re = 0 -- Enable new regexp engine to stop syntax highlighting breaking on fast scrolls

