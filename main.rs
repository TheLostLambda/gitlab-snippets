extern crate evdev_rs as evdev;

use evdev::*;
use evdev::enums::*;
use std::fs::File;
use std::thread::sleep;
use std::time::Duration;

fn usage() {
    println!("Usage: evtest /path/to/device");
}

fn main() {
    let mut args = std::env::args();

    if args.len() != 2 {
        usage();
        std::process::exit(1);
    }

    let path = &args.nth(1).unwrap();
    let f = File::open(path).unwrap();

    let d = Device::new_from_fd(f).unwrap();

    // I'm duplicating my existing keyboard.
    let i = UInputDevice::create_from_device(&d).unwrap();
    
    sleep(Duration::new(1,0)); // Wait a second before running the key commands
    
    // Switching workspaces is Ctrl+Alt+Down (Arrow keys)
    let t = TimeVal::new(0,0);
    let events = vec![InputEvent::new(&t, &EventCode::EV_KEY(EV_KEY::KEY_LEFTCTRL), 1),
                      InputEvent::new(&t, &EventCode::EV_KEY(EV_KEY::KEY_LEFTALT), 1),
                      InputEvent::new(&t, &EventCode::EV_KEY(EV_KEY::KEY_DOWN), 1),
                      InputEvent::new(&t, &EventCode::EV_SYN(EV_SYN::SYN_REPORT), 0), //This line is needed to flush the keys added before it
                      InputEvent::new(&t, &EventCode::EV_KEY(EV_KEY::KEY_DOWN), 0),
                      InputEvent::new(&t, &EventCode::EV_KEY(EV_KEY::KEY_LEFTCTRL), 0),
                      InputEvent::new(&t, &EventCode::EV_KEY(EV_KEY::KEY_LEFTALT), 0),
                      InputEvent::new(&t, &EventCode::EV_SYN(EV_SYN::SYN_REPORT), 0),
                      InputEvent::new(&t, &EventCode::EV_KEY(EV_KEY::KEY_CALC), 1),
                      InputEvent::new(&t, &EventCode::EV_SYN(EV_SYN::SYN_REPORT), 0),
                      InputEvent::new(&t, &EventCode::EV_KEY(EV_KEY::KEY_CALC), 0),
                      InputEvent::new(&t, &EventCode::EV_SYN(EV_SYN::SYN_REPORT), 0),
    ];

    for e in events {
        i.write_event(&e).expect("whoops");
    }
}