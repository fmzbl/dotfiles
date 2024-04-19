# Stuff to make stuff work

# Audio Interface (UMC202HD)
echo "options snd_usb_audio quirk_flags=0x20010" | sudo tee -a /etc/modprobe.d/snd_usb_audio.conf
# Restart pipewire when it doesnt recognize the device
systemctl --user restart pipewire
