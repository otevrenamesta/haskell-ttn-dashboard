Connected: Online
Disconnected: Offline
ConnectedClients n@Int: Clients connected: #{show n}
Direction: Direction
Payload: Payload
Time: Time
Port: Port
Counter: Counter
Network: Network
Settings: Settings
MessageCount n@Int: Message count: #{show n}
Activation: Activation
Uplink: Uplink
DownLink: DownLink
TimeFormat: Time format
TFTime: Time
TFDateTime: Date and time
Temperature t@Float: Temperature: #{showFloat t}
Humidity h@Float: Humidity: #{showFloat h}
Home: Home
Docs: Documentation
About: About
Device: Device
Application: Application
LastReadings: Last readings
Frequency f@Double: Frequency: #{showFloat f}
DataRate x@Text: Data rate #{T.unpack x}
RSSISNR rssi@Double snr@Double: RSSI: #{showFloat rssi}dBm SNR: #{showFloat snr}dB
Channel chan@Double: Channel: #{show $ round chan}

Gateways: Gateways
NMnM i@Int: Nové Město na Moravě #{show i}
BaseGWOutdoor: base48.cz outdoor gateway
BaseGWIndoor: base48.cz indoor gateway
UnknownGw: Unknown gateway

Language: Language
LangCzech: Czech
LangSlovak: Slovak
LangEnglish: English

EnglishResources: English resources
CzechResources: Czech resources

HomeIntro: This application represents a technological demo of open LoRaWAN IoT network, feeding live data from LoRaWAN sensor connected to
HomeSensor: Device periodically sends binary packed temperature and humidity values from SHT22 I2C sensor.
HomeAPI: Messages are marshalled from TTN MQTT API to Websocket server for this client application.
TTN: The Things Network

AboutPowered: Application is powered by
AboutPowered2: and excellent
AboutMiso: Miso framework
AboutSensor: Sensor node firmware is also written in Haskell using
AboutIvoryTower: Ivory Tower framework
AboutGateways: Gateways built by
AboutHosted: Hosted on
AboutBy: Made by
AboutInCoOp: in cooperation with
AboutSources: Source code
AboutThisApp: (this application)
