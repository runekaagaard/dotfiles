set nocompatible              " be iMproved, required
filetype off                  " required

""" Setup vundle

" Set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Installed plugins
Plugin 'scrooloose/nerdtree'
Bundle 'jistr/vim-nerdtree-tabs'
Plugin 'ctrlp.vim' 
Plugin 'davidhalter/jedi-vim'
Plugin 'jwhitley/vim-colors-solarized'
Plugin 'cespare/vim-sbd'
Plugin 'scrooloose/syntastic'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

""" Colors and syntax highlighting

syntax enable
set background=dark
colorscheme solarized

""" Tabs

set tabstop=4       " number of visual spaces per TAB
set shiftwidth=4    " number of spaces to use for auto indent
set softtabstop=4   " number of spaces in tab when editing
set expandtab       " tabs are spaces

""" UI

set number              " show line numbers
set cursorline          " highlight current line
filetype indent on      " load filetype-specific indent files
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set showmatch           " highlight matching [{()}]
set ttyfast                     " faster redraw
set colorcolumn=79
" always open help in new tab 
cabbrev help tab help 
cabbrev he tab help 
cabbrev h tab help 

" Enable mouse use in all modes
set mouse=a
"
" Set this to the name of your terminal that supports mouse codes.
" Must be one of: xterm, xterm2, netterm, dec, jsbterm, pterm
set ttymouse=xterm2

" Use CTRL-D for saving, also in Insert mode
noremap <C-D> :update<CR>
vnoremap <C-D> <C-C>:update<CR>
inoremap <C-D> <C-O>:update<CR>

""" Command line

set noruler
set noshowcmd
set showtabline=2 " Always display the tabline, even if there is only one tab
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set laststatus=2 " Always display the statusline in all windows

""" Leader

let mapleader=","
inoremap jk <esc> " jk is escape
inoremap kj <esc>:w<CR> " kj escapses and saves

""" Powerline

set encoding=utf-8
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

""" ctrl+p

set runtimepath^=~/.vim/bundle/ctrlp.vim

""" nerdtree

let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMouseMode=3
map <C-m> :NERDTreeTabsFind<CR>
map <C-n> :NERDTreeSteppedOpen<CR>
unmap <CR>

""" Tabs

map <C-k> :bp<cr>
map <C-l> :bn<cr>

""" Jedi-vim

let g:jedi#show_call_signatures = 2

" https://github.com/cespare/vim-sbd
map <S-w> :Sbd<CR>
map <S-q> :Sbdm<CR>

""" ctrlp

" Enter opens in a new tab
let g:ctrlp_prompt_mappings = {
    \ 'AcceptSelection("e")': ['<c-t>'],
    \ 'AcceptSelection("t")': ['<cr>', '<2-LeftMouse>'],
    \ }

""" syntastic

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_quiet_messages = { "type": "style" }
