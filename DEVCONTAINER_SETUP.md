# 🛠️ Dev Container Setup

## Что установлено

После пересборки dev контейнера будут доступны:

### Node.js Tools
- ✅ `node` - Node.js LTS
- ✅ `npm` - Package manager
- ✅ `wscat` - WebSocket CLI client

### Python Tools
- ✅ `python3` - Python 3.11
- ✅ `pip3` - Package manager
- ✅ `websockets` - WebSocket library

### Network Tools
- ✅ `curl` - HTTP client
- ✅ `wget` - File downloader
- ✅ `jq` - JSON processor
- ✅ `netcat` - Network utility

### Build Tools
- ✅ `build-essential` - GCC, make, etc
- ✅ `pkg-config` - Package configuration
- ✅ `libssl-dev` - OpenSSL development

## Как пересобрать контейнер

### Через Command Palette (рекомендуется)

1. Нажмите `Ctrl+Shift+P` (или `Cmd+Shift+P` на Mac)
2. Введите: `Dev Containers: Rebuild Container`
3. Нажмите Enter
4. Дождитесь завершения (3-5 минут)

### Через меню

1. Нажмите на зеленую кнопку в левом нижнем углу
2. Выберите `Rebuild Container`

## После пересборки

Проверьте установку:

```bash
# Node.js
node --version
npm --version

# Python
python3 --version
pip3 --version

# WebSocket tools
wscat --version
```

## Быстрый старт с логами

После пересборки запустите:

```bash
# Способ 1: wscat
wscat -c wss://vibee-mcp.fly.dev/ws/logs

# Способ 2: Python
python3 test_ws.py

# Способ 3: Автоматический выбор
./watch-logs.sh

# Способ 4: HTML в браузере
open test_ws.html
```

## Troubleshooting

### Контейнер не пересобирается

1. Закройте все терминалы
2. Перезапустите VS Code
3. Попробуйте снова

### Команды не найдены после пересборки

Проверьте, что вы в правильном контейнере:
```bash
echo $REMOTE_CONTAINERS
```

Должно вывести что-то вроде `true`.

### npm/pip не работают

Перезапустите терминал:
```bash
exit
# Откройте новый терминал
```

## Альтернатива: Установка вручную

Если не хотите пересобирать контейнер:

```bash
# Установите Node.js
curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
sudo apt-get install -y nodejs

# Установите wscat
npm install -g wscat

# Установите Python packages
pip3 install websockets requests
```

## Что дальше?

После установки инструментов:

1. **Смотрите логи**: `./watch-logs.sh`
2. **Читайте документацию**: `LOGS_PROTOCOL.md`
3. **Используйте dashboard**: [https://vibee-mcp.fly.dev/dashboard](https://vibee-mcp.fly.dev/dashboard)

## Полезные команды

```bash
# Смотреть логи через wscat
wscat -c wss://vibee-mcp.fly.dev/ws/logs

# Смотреть логи через Python
python3 test_ws.py

# Смотреть логи с форматированием JSON
wscat -c wss://vibee-mcp.fly.dev/ws/logs | jq .

# Сохранить логи в файл
wscat -c wss://vibee-mcp.fly.dev/ws/logs > logs.txt

# Смотреть логи с фильтром
wscat -c wss://vibee-mcp.fly.dev/ws/logs | grep "TG:"
```

## Итог

После пересборки контейнера у вас будет полный набор инструментов для работы с Vibee! 🚀
