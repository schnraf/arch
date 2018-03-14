[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
setxkbmap -layout ch
xsetroot -cursor_name left_ptr
unclutter -idle 5 &
urxvtd -q -o -f &
thunar --daemon &
notificathor &
mpd &
hsetroot -fill /home/zara/pictures/wallpaper/login.jpg
nitrogen --restore &
compton -cCfb -D 4 -r 5 -l -5 -t -2 -o 0.2 -e 0.6 &
mopidy &
redshift &
dunst &
xmobar &
#sleep 3 && conky -c ~/.conkympd &
#sleep 3 && stalonetray --geometry 1x1-1-1 --transparent true &
#sleep 10 && dropboxd &

# Settings for Japanese input
export GTK_IM_MODULE='uim'
export QT_IM_MODULE='uim'
uim-xim &
export XMODIFIERS='@im=uim'


exec dbus-launch xmonad
#exec openbox
