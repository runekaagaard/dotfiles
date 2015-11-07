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
Plugin 'majutsushi/tagbar'
Plugin 'rking/ag.vim'
Plugin 'mileszs/ack.vim'
Plugin 'ap/vim-buftabline'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

""" Colors and syntax highlighting

syntax enable
colorscheme solarized
set background=dark

""" Tabs

set tabstop=4       " number of visual spaces per TAB
set shiftwidth=4    " number of spaces to use for auto indent
set softtabstop=4   " number of spaces in tab when editing
set expandtab       " tabs are spaces

""" UI

set hidden
set number              " show line numbers
set cursorline          " highlight current line
filetype indent on      " load filetype-specific indent files
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set showmatch           " highlight matching [{()}]
"set ttyfast             " faster redraw
set colorcolumn=79      " vertical ruler 
set backspace=2         " allow deletion in insert mode
set ignorecase

" Help always opens in a new tab to not mess with the current layout
cabbrev help tab help
cabbrev he tab help
cabbrev h tab help

" Enable mouse use in all modes
set mouse=a
"
" Set this to the name of your terminal that supports mouse codes.
" Must be one of: xterm, xterm2, netterm, dec, jsbterm, pterm
if !has('nvim')
    set ttymouse=xterm2
endif
set wildignore+=.pyc
set wildignore+=.orig

" Use tab and shif tab to indent lines, 

vnoremap <s-tab> <gv
vnoremap <tab> >gv
nnoremap <s-tab> <<
nnoremap <tab> >>

"This unsets the last search pattern register by hitting return
"nnoremap <CR> :noh<CR><CR>

" Use enter and backspace to add and delete lines.

nnoremap <CR> o <esc> 0k
nnoremap <BS> "_dd <esc> 0k
 
""" Command line
 
set noruler
set noshowcmd
set showtabline=2 " Always display the tabline, even if there is only one tab
"""set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set laststatus=2 " Always display the statusline in all windows

""" Leader

let mapleader=","

""" Leader commands

nnoremap <leader>t :split <CR> gg

""" Leaving insert mode.
inoremap jk <esc> " jk is escape
inoremap kj <esc>:w<CR> " kj escapses and saves
inoremap lkj <esc> :w<CR> :q<CR> " lkj escapses and saves and quits

""" Powerline

"set encoding=utf-8
"python from powerline.vim import setup as powerline_setup
"python powerline_setup()
"python del powerline_setup

""" ctrl+p

set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_custom_ignore = '\vbuild/|dist/|venv/|target/|\.(o|swp|pyc|egg)$'
let g:ctrlp_open_func = { 'files': 'CustomOpenFunc' }
function! CustomOpenFunc(action, line)
    call call('ctrlp#acceptfile', [':t', a:line])
endfunction

""" nerdtree

let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMouseMode=3
let NERDTreeIgnore = ['\.pyc$', 'build', 'venv', 'egg', 'egg-info/', 'dist']
let NERDTreeChDirMode=0
map <C-d> :NERDTreeClose<CR>
map <C-n> :NERDTreeSteppedOpen<CR>
"unmap <CR>

""" Tabs

map <C-k> :bp<cr>
map <C-l> :bn<cr>

""" Jedi-vim

let g:jedi#show_call_signatures = 2

" https://github.com/cespare/vim-sbd
map <S-w> :Sbd<CR>
map <S-q> :Sbdm<CR>

""" syntastic

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_quiet_messages = { "type": "style" }
let g:syntastic_full_redraws = 1

""" Tagbar
nmap <C-d> :TagbarToggle<CR>
let g:tagbar_foldlevel = 0 

""" virtual env
if filereadable($VIRTUAL_ENV . '/.vimrc')
    source $VIRTUAL_ENV/.vimrc
endif

""" buttabline

let g:buftabline_indicators = 1

""" Ack
cabbrev A Ack! --ignore-file=is:tags

" Mapping xclip clipboard support
if has("unix")
  let s:uname = system("uname -s")
  if s:uname =~ "Darwin"
    " On OSX
    vmap <C-c> y:call system("pbcopy", getreg("\""))<CR>
    nmap <C-v> :call setreg("\"",system("pbpaste"))<CR>
  endif
  if s:uname =~ "Linux"
    " On Linux
    " Without X server it will just use a temporal file
    if system("echo $DISPLAY") =~ ""
      vmap <C-c> y: call system("> /tmp/theClipboardWithoutX", getreg("\""))<CR>
      map <C-v> :call setreg("\"", system("< /tmp/theClipboardWithoutX"))<CR>p
    else
      vmap <C-c> y: call system("xclip -i -selection clipboard", getreg("\""))<CR>
      map <C-v> :call setreg("\"",system("xclip -o -selection clipboard"))<CR>p
    endif
  endif
endif 
