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
set colorcolumn=79      " vertical ruler 
set backspace=2         " allow deletion in insert mode
set ignorecase
" No automatic comments
:set formatoptions-=cro

" Help always opens in a new tab to not mess with the current layout
cabbrev help tab help
cabbrev he tab help
cabbrev h tab help

" Enable mouse use in all modes
set mouse=a
 
" Set this to the name of your terminal that supports mouse codes.
" Must be one of: xterm, xterm2, netterm, dec, jsbterm, pterm
if !has('nvim')
    set ttymouse=xterm2
endif
 
" Some wildignores. Why doesn't nerdtree and ack not support this? 
set wildignore+=.pyc
set wildignore+=.orig

""" Heretic 'normal' editor behaviour.

" backspace and cursor keys wrap to previous/next line
set backspace=indent,eol,start whichwrap+=<,>,[,]

" backspace in Visual mode deletes selection
vnoremap <BS> d

" Backspace is backspace in command mode.
"function! RBackSpace()
"    let c = col('.')
"    if c == 1 
"        execute "normal kJ0" 
"    else
"        execute "normal Xh" 
"    endif
"endfunction
"nnoremap <silent> <BS> :call RBackSpace()<CR> 

" Enter inserts a new line
nnoremap <CR> O<esc><down>

" Insert linebreak at cursor location.
:nnoremap K i<CR><Esc><right> 

" Use tab and shif tab to indent lines.
vnoremap <s-tab> <gv
vnoremap <tab> >gv
nnoremap <s-tab> <<
nnoremap <tab> >> 

" Alt-d deletes line above.
nnoremap ∂ k_dd^

" Ctrl-d deletes line.
nnoremap <C-D> _dd^

" Space is space in command mode."
nnoremap <space> i<space><esc>l

" alt-j/k moves lines.
nnoremap ‹ :m .+1<CR>==
nnoremap ∆ :m .-2<CR>==
inoremap ‹ <Esc>:m .+1<CR>==gi
inoremap ∆ <Esc>:m .-2<CR>==gi
vnoremap ‹ :m '>+1<CR>gv=gv
vnoremap ∆ :m '<-2<CR>gv=gv<Paste>

" alt-arrows moves lines.
nnoremap <F24> :m .+1<CR>==
nnoremap <F23> :m .-2<CR>==
inoremap <F24> <Esc>:m .+1<CR>==gi
inoremap <F23> <Esc>:m .-2<CR>==gi
vnoremap <F24> :m '>+1<CR>gv=gv
vnoremap <F23> :m '<-2<CR>gv=gv<Paste>

" CTRL-A is Select all
noremap <C-A> gggH<C-O>G
inoremap <C-A> <C-O>gg<C-O>gH<C-O>G
cnoremap <C-A> <C-C>gggH<C-O>G
onoremap <C-A> <C-C>gggH<C-O>G
snoremap <C-A> <C-C>gggH<C-O>G
xnoremap <C-A> <C-C>ggVG
 
" shift+arrow selection
nmap <S-Up> V^
nmap <S-Down> V^
nmap <S-Left> v<Left>
nmap <S-Right> v<Right>
vmap <S-Up> <Up>^
vmap <S-Down> <Down>^
vmap <S-Left> <Left>
vmap <S-Right> <Right>
imap <S-Up> <Esc>V^
imap <S-Down> <Esc>V^
imap <S-Left> <Esc><Right>v<Left>
imap <S-Right> <Esc><Right>v<Right>

" Use CTRL-S for saving, also in Insert mode
noremap <C-S>		:update<CR>
vnoremap <C-S>		<C-C>:update<CR>
inoremap <C-S>		<C-O>:update<CR>

""" Command line
 
set noruler
set noshowcmd
set showtabline=2 " Always display the tabline, even if there is only one tab
set laststatus=2 " Always display the statusline in all windows

""" Leader

let mapleader=","

""" Leader commands

" Clear search highlighting. 
nnoremap <leader><leader> :noh<CR><CR>
" Open a split with the same file scrolled to the top for imports.
nnoremap <leader>t :split <CR> gg
" Save and reload vimrc. 
nnoremap <leader>r :w <CR> :source ~/.vimrc <CR>
 
""" Leaving insert mode.
inoremap jk <esc> " jk is escape
inoremap kj <esc>:w<CR> " kj escapses and saves
inoremap lkj <esc> :w<CR> :q<CR> " lkj escapses and saves and quits

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
let NERDTreeIgnore = ['\.pyc$', 'build', 'venv', 'egg', 'egg-info/', 'dist', '\.orig$']
let NERDTreeChDirMode=0
map <C-n> :NERDTreeSteppedOpen<CR>

""" Tabs

map <C-k> :bp<cr>
map <C-l> :bn<cr>
map <C-left> :bp<cr>
map <C-right> :bn<cr>

""" Jedi-vim

let g:jedi#show_call_signatures = 2

""" Close buffer 'tabs'. 
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
nnoremap <Leader>d :TagbarToggle<CR>
let g:tagbar_foldlevel = 0 

""" virtual env
if filereadable($VIRTUAL_ENV . '/.vimrc')
    source $VIRTUAL_ENV/.vimrc
endif

""" buttabline
let g:buftabline_indicators = 1

""" Ack
cabbrev A Ack! --ignore-file=is:tags
