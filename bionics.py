import os
import sys
import csv
import time
from pynput import keyboard

from common import *
from myo_raw import MyoRaw

class Myo(MyoRaw):

    def __init__(self, tty=None):
        MyoRaw.__init__(self, tty)
        self.paused = False
        self.repeat = 0
        self.gesture = 0
        self.emg = [0] * 8
        self.imu = [0] * 10
        self.custom_row = []
        self.updated = False
        self.add_emg_handler(self.emg_handler)
        self.add_imu_handler(self.imu_handler)
        self.connect()
        self.start_time = time.time()

    def pause(self):
        self.paused ^= True

    def next_repeat(self):
        self.repeat += 1
        self.start_time = time.time()
        self.custom_row = ['Repeat ' + str(self.repeat)]

    def next_gesture(self):
        self.gesture += 1
        self.repeat = 0

    def emg_handler(self, emg, _):
        self.updated = True
        self.emg = list(emg)

    def imu_handler(self, quat, acc, gyro):
        self.imu = list(quat) + list(acc) + list(gyro)

    def write_data(self):
        self.updated = False
        print(self.emg)
        print(self.imu)
        path = 'data/emgData-G' + str(self.gesture) + '.csv'
        with open(path, 'a') as csvFile:
            writer = csv.writer(csvFile)
            t = int((time.time() - self.start_time) * 1000)
            row = [t] + self.emg + self.imu
            if os.path.getsize(path) == 0:
                row = ['time', 'emg0', 'emg1', 'emg2', 'emg3', 'emg4', 'emg5', 'emg6', 'emg7', 'quat0',
                       'quat1', 'quat2', 'quat3', 'acc0', 'acc1', 'acc2', 'gyro0', 'gyro2', 'gyro2']
            if len(self.custom_row) > 0:
                row = [t] + self.custom_row
                self.custom_row = []
            writer.writerow(row)

if __name__ == '__main__':
    m = Myo(sys.argv[1] if len(sys.argv) >= 2 else None)

    with keyboard.GlobalHotKeys({
            'q': sys.exit,
            'p': m.pause,
            'r': m.next_repeat,
            'g': m.next_gesture
    }) as h:
        while h.isAlive():
            m.run()
            if m.paused:
                continue
            if m.updated:
                m.write_data()
        h.join()
