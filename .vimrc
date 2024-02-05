" font -> Menlo

" --- General
syntax on

colorscheme zaibatsu
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
set wildmenu
set wildignorecase
set wildignore=\*.git\*
set backspace=2
set expandtab

" --- Search
:set hlsearch
:nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>
