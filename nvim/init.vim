" --- General  ---

syntax on

set number
set ruler
set tabstop=4 softtabstop=4
set shiftwidth=4
set smartindent
set nowrap
set incsearch
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set relativenumber
set hidden
set encoding=UTF-8
set mouse=a
set guicursor=i:block

" Yank to clipboard
nnoremap <C-y> "+y
vnoremap <C-y> "+y
nnoremap <C-p> "+gP
vnoremap <C-p> "+gP

" --- Plugins  ---

call plug#begin('~/.config/nvim/plugged')

Plug 'jacoborus/tender.vim'

Plug 'kyazdani42/nvim-tree.lua' 
Plug 'kyazdani42/nvim-web-devicons'

Plug 'nvim-telescope/telescope.nvim' 
Plug 'nvim-lua/plenary.nvim'

" Lsp and autocomplete
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'

call plug#end()

" --- Colors ---

set background=dark
if (has("termguicolors"))
 set termguicolors
endif

colorscheme tender

" --- Flie tree ---
:lua require("nvim-tree").setup()

" --- Telescope ---
:lua require('telescope').setup 
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" --- LSP and Autocomplete  ---

command! Fmt execute 'lua vim.lsp.buf.formatting()'

:lua << EOF
  -- Setup nvim-cmp.
  local cmp = require'cmp'

  cmp.setup({
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      end,
    },
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'vsnip' }, 
    }, {
      { name = 'buffer' },
    })
  })

  -- Set configuration for specific filetype.
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
      { name = 'buffer' },
    })
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })

  local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

	
  -- Lspconfig
  local on_attach = function(client, bufnr)
  	-- Mappings.
  	local bufopts = { noremap=true, silent=true, buffer=bufnr }
  	vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  	vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  	vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  	vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  	vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  	vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  	vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  	vim.keymap.set('n', '<space>wl', function()
  		print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  	end, bufopts)
  	vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  	vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  	vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  	vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  	vim.keymap.set('n', '<space>f', vim.lsp.buf.formatting, bufopts)
  end

  -- load lsp servers
  local nvim_lsp = require('lspconfig')
  local servers = { 'clangd', 'intelephense' }
  for _, lsp in ipairs(servers) do
	nvim_lsp[lsp].setup {
   	  capabilities = capabilities,
  	  on_attach = on_attach,
	}
  end
EOF
