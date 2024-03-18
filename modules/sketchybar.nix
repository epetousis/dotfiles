{ config, pkgs, lib, ... }:
let
  space = pkgs.writeScript "space.sh" ''
    #!/bin/sh
    sketchybar --set "$NAME" background.drawing="$SELECTED"
  '';

  frontApp = pkgs.writeScript "front_app.sh" ''
  #!/bin/sh
  if [ "$SENDER" = "front_app_switched" ]; then
    sketchybar --set "$NAME" label="$INFO"
  fi
  '';

  clock = pkgs.writeScript "clock.sh" ''
    #!/bin/sh
    sketchybar --set "$NAME" label="$(date '+%d/%m %H:%M')"
  '';

  volume = pkgs.writeScript "volume.sh" ''
    #!/bin/sh
    if [ "$SENDER" = "volume_change" ]; then
      VOLUME="$INFO"

      case "$VOLUME" in
        [6-9][0-9]|100) ICON="󰕾"
        ;;
        [3-5][0-9]) ICON="󰖀"
        ;;
        [1-9]|[1-2][0-9]) ICON="󰕿"
        ;;
        *) ICON="󰖁"
      esac

      sketchybar --set "$NAME" icon="$ICON" label="$VOLUME%"
    fi
  '';

  battery = pkgs.writeScript "battery.sh" ''
    #!/bin/sh

    PERCENTAGE="$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)"
    CHARGING="$(pmset -g batt | grep 'AC Power')"

    if [ "$PERCENTAGE" = "" ]; then
      exit 0
    fi

    case "''${PERCENTAGE}" in
      9[0-9]|100) ICON=""
      ;;
      [6-8][0-9]) ICON=""
      ;;
      [3-5][0-9]) ICON=""
      ;;
      [1-2][0-9]) ICON=""
      ;;
      *) ICON=""
    esac

    if [[ "$CHARGING" != "" ]]; then
      ICON=""
    fi

    # The item invoking this script (name $NAME) will get its icon and label
    # updated with the current battery status
    sketchybar --set "$NAME" icon="$ICON" label="''${PERCENTAGE}%"
  '';
in
{
  options = {
    services.evanSketchybar = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = lib.mdDoc ''
          Whether to enable Sketchybar with my sane defaults.
        '';
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf config.services.evanSketchybar.enable {
    services.sketchybar = {
    enable = true;
    config = ''
      # For all options see:
      # https://felixkratz.github.io/SketchyBar/config

      ##### Bar Appearance #####

      sketchybar --bar position=top height=30 blur_radius=30 color=0x40000000 corner_radius=9 margin=10 y_offset=10

      ##### Changing Defaults #####

      default=(
        padding_left=5
        padding_right=5
        icon.font="FiraCode Nerd Font:Bold:17.0"
        label.font="FiraCode Nerd Font:Bold:14.0"
        icon.color=0xffffffff
        label.color=0xffffffff
        icon.padding_left=4
        icon.padding_right=4
        label.padding_left=4
        label.padding_right=4
      )
      sketchybar --default "''${default[@]}"

      ##### Mission Control Space Indicators #####

      SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")
      for i in "''${!SPACE_ICONS[@]}"
      do
        sid="$(($i+1))"
        space=(
          space="$sid"
          icon="''${SPACE_ICONS[i]}"
          icon.padding_left=7
          icon.padding_right=7
          background.color=0x40ffffff
          background.corner_radius=5
          background.height=25
          label.drawing=off
          script="${space}"
          click_script="yabai -m space --focus $sid"
            )
        sketchybar --add space space."$sid" left --set space."$sid" "''${space[@]}"
      done

      ##### Adding Left Items #####

      sketchybar --add item chevron left \
                 --set chevron icon= label.drawing=off \
                 --add item front_app left \
                 --set front_app icon.drawing=off script="${frontApp}" \
                 --subscribe front_app front_app_switched

      ##### Adding Right Items #####

      sketchybar --add item clock right \
                 --set clock update_freq=10 icon=  script="${clock}" \
                 --add item volume right \
                 --set volume script="${volume}" \
                 --subscribe volume volume_change \
                 --add item battery right \
                 --set battery update_freq=120 script="${battery}" \
                 --subscribe battery system_woke power_source_change
      sketchybar --update
      echo "sketchybar configuration loaded.."
    '';
    };
    system.defaults.NSGlobalDomain._HIHideMenuBar = config.services.sketchybar.enable;
    })
  ];
}
