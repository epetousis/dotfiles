-- vim: set tabstop=2:softtabstop=2:shiftwidth=2:expandtab:smarttab
local Plug = vim.fn['plug#']

vim.call('plug#begin')
  Plug 'vimwiki/vimwiki'
  -- Begin org mode related plugins
  Plug 'nvim-treesitter/nvim-treesitter'
  Plug 'nvim-orgmode/orgmode'
  Plug 'akinsho/org-bullets.nvim'
  -- End org mode related plugins
  Plug('michaelb/sniprun', {['do'] = 'bash install.sh'})
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
vim.call('plug#end')

-- Set up comment.nvim bindings
require('Comment').setup()

-- Label mode for vim-sneak
vim.g["sneak#label"] = 1

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
vim.opt.timeoutlen = 500

require("which-key").setup {
}

vim.opt.splitbelow = true -- open window splits below

vim.opt.laststatus = 2 -- enable statusline
vim.opt.statusline = "%f %m %r %l,%c %= %p%%" -- filename, modified flag, readonly flag, line/column number, file percentage on right

vim.g.re = 0 -- Enable new regexp engine to stop syntax highlighting breaking on fast scrolls

-- Language servers
vim.g.coq_settings = { ['auto_start'] = 'shut-up' }
local coq = require "coq"
require'lspconfig'.tsserver.setup(coq.lsp_ensure_capabilities{})
require'lspconfig'.eslint.setup(coq.lsp_ensure_capabilities{})
require'lspconfig'.pyright.setup(coq.lsp_ensure_capabilities{})
require'lspconfig'.vuels.setup(coq.lsp_ensure_capabilities{})
require'lspconfig'.rust_analyzer.setup(coq.lsp_ensure_capabilities{})

-- Setup for nvim-orgmode
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'f110024d539e676f25b72b7c80b0fd43c34264ef',
    files = {'src/parser.c', 'src/scanner.cc'},
  },
  filetype = 'org',
}

require'nvim-treesitter.configs'.setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    disable = {'org'}, -- Remove this to use TS highlighter for some of the highlights (Experimental)
    additional_vim_regex_highlighting = {'org'}, -- Required since TS highlighter doesn't support all syntax features (conceal)
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}

require('orgmode').setup({
  org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
  org_default_notes_file = '~/Dropbox/org/refile.org',
})

