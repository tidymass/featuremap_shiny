# FeatureMAP Shiny 部署手册

课题组内部部署在实验室工作站上。所有东西都在 `~/featuremap-deploy/` 下，**不需要 sudo**。

- **应用地址（发给组员）**：http://10.96.129.143:3838
- **服务器**：`yuting@10.96.129.143`，Ubuntu 24.04，128 核 / 503G 内存 / 3× RTX PRO 6000
- **仓库**：`git@github.com:tidymass/featuremap_shiny.git`（私有，服务器用只读 deploy key 拉取）
- **本地代码**：`/Users/yuting/Documents/AtNTUlife/ShenLab/1_MyProject/featureFMA/featuremap_shiny`

---

## 0. 最常用的三条

```bash
# 登录服务器
ssh yuting@10.96.129.143

# 看服务状态
~/featuremap-deploy/src/deploy/run_app.sh status

# 发布更新（代码已 push 之后）
~/featuremap-deploy/src/deploy/run_app.sh update
```

---

## 1. 登录服务器

```bash
ssh yuting@10.96.129.143
```

**配置免密登录**（只需做一次，之后不再输密码）：

```bash
# 在 Mac 上执行
ssh-copy-id yuting@10.96.129.143
```

**怎么判断当前在哪台机器**（容易搞混）：

```bash
hostname
# workstation            → 在服务器上
# Yutings-MacBook-Pro    → 在 Mac 上
```

在服务器上想回到 Mac，输 `exit`。

**从 Mac 传文件到服务器**：

```bash
scp 本地文件 yuting@10.96.129.143:~/目标路径
```

**不登录、直接在服务器上跑一条命令**：

```bash
ssh yuting@10.96.129.143 '~/featuremap-deploy/src/deploy/run_app.sh status'
```

---

## 2. 服务器上的目录结构

```
~/featuremap-deploy/
├── src/              本仓库的 git checkout（部署用的源码）
│   └── deploy/       这些脚本
├── rlib/             R 包库，与你个人的 R 环境隔离
├── local/            自己编译的 GLPK（igraph 需要）
├── bin/              pandoc 静态二进制
├── logs/             安装日志 + app.log（应用运行日志）
├── env.sh            环境变量
├── Renviron.install  强制 R 库路径用的配置
└── app.pid           运行中进程的 PID
```

---

## 3. 日常运维

所有命令都在服务器上执行。为方便，可以先进目录：

```bash
cd ~/featuremap-deploy/src
```

```bash
./deploy/run_app.sh status     # 是否在跑、版本、commit、运行时长
./deploy/run_app.sh logs       # 实时跟踪日志（Ctrl+C 退出，不影响服务）
./deploy/run_app.sh start      # 启动
./deploy/run_app.sh stop       # 停止
./deploy/run_app.sh restart    # 重启
./deploy/run_app.sh update     # 拉取 + 重装 + 重启
```

**看历史日志**（每次启动会把上一份改名保存）：

```bash
ls -lt ~/featuremap-deploy/logs/
tail -100 ~/featuremap-deploy/logs/app.log
```

**服务器重启后应用不会自动起来**，需要手动：

```bash
ssh yuting@10.96.129.143 '~/featuremap-deploy/src/deploy/run_app.sh start'
```

---

## 4. 更新应用（最常做的事）

### 完整流程

```bash
# ===== 在 Mac 上 =====
cd /Users/yuting/Documents/AtNTUlife/ShenLab/1_MyProject/featureFMA/featuremap_shiny

# 1. 改代码（inst/app/app.R、R/*.R 等），本地测试
#    R 里：pkgload::load_all("."); shiny::runApp("inst/app")

# 2. 看看改了什么
git status
git diff

# 3. 提交并推送
git add -A
git commit -m "描述这次改动"
git push

# ===== 在服务器上 =====
ssh yuting@10.96.129.143
~/featuremap-deploy/src/deploy/run_app.sh update

# 4. 确认生效
~/featuremap-deploy/src/deploy/run_app.sh status
#    看 "version" 和 "source commit" 是否是你刚推的
```

`update` 依次做四件事：`git pull` → `install_deps`（补齐新增依赖）→ `R CMD INSTALL` → **重启**。

**重启这一步不能省。** 运行中的 R 进程把启动时加载的代码握在内存里，只重装包不重启，用户看到的还是旧版。同时重启会**踢掉正在跑分析的人**，所以挑没人用的时候做，或者先 `status` 看一眼。

### 只改了依赖包（如 pcsf2featuremap）

`remotes` 靠比对 commit SHA 决定要不要重装远程依赖，可能会跳过。强制重装：

```bash
cd ~/featuremap-deploy/src
R_ENVIRON_USER=~/featuremap-deploy/Renviron.install \
R_PROFILE_USER=~/featuremap-deploy/Rprofile.install.R \
FM_CRAN=https://packagemanager.posit.co/cran/__linux__/noble/latest \
FM_LIB=~/featuremap-deploy/rlib \
  Rscript -e 'remotes::install_github("tidymass/pcsf2featuremap", lib=.libPaths()[1], force=TRUE)'
./deploy/run_app.sh restart
```

### 想先在服务器上试改动，不走 git

```bash
# Mac 上直接传文件
scp inst/app/app.R yuting@10.96.129.143:~/featuremap-deploy/src/inst/app/app.R
ssh yuting@10.96.129.143
cd ~/featuremap-deploy/src
R CMD INSTALL -l ~/featuremap-deploy/rlib --no-multiarch --with-keep.source .
./deploy/run_app.sh restart
```

⚠️ 这样服务器上的 checkout 就和 git 不一致了，下次 `update` 时 `git pull --ff-only` 会失败。用完记得还原：

```bash
cd ~/featuremap-deploy/src && git checkout -- . && git pull
```

---

## 5. 修改部署配置

配置集中在 `deploy/run_app.sh` 顶部：

```bash
export FM_HOST="${FM_HOST:-0.0.0.0}"
export FM_PORT="${FM_PORT:-3838}"
export FEATUREMAP_MAX_UPLOAD_MB="${FEATUREMAP_MAX_UPLOAD_MB:-500}"
export FEATUREMAP_AUTH_ENABLED="${FEATUREMAP_AUTH_ENABLED:-false}"
export FEATUREMAP_AUTH_DB="${FEATUREMAP_AUTH_DB:-$FM_ROOT/auth/featuremap_auth.sqlite}"
export FEATUREMAP_PUBLIC_URL="${FEATUREMAP_PUBLIC_URL:-}"
export FEATUREMAP_SMTP_HOST="${FEATUREMAP_SMTP_HOST:-smtp.gmail.com}"
export FEATUREMAP_SMTP_PORT="${FEATUREMAP_SMTP_PORT:-587}"
export FEATUREMAP_SMTP_USERNAME="${FEATUREMAP_SMTP_USERNAME:-featuremapofficial@gmail.com}"
export FEATUREMAP_SMTP_FROM="${FEATUREMAP_SMTP_FROM:-featuremapofficial@gmail.com}"
# export FEATUREMAP_SMTP_PASSWORD=your-gmail-app-password
# export SILICONFLOW_BASE_URL=https://api.siliconflow.cn
```

- **`FM_HOST`** —— 必须是 `0.0.0.0`，别人才连得上。Shiny 默认的 `127.0.0.1` 只接受服务器本机连接。
- **`FM_PORT`** —— 换端口后组员的地址也要跟着变。
- **`FEATUREMAP_MAX_UPLOAD_MB`** —— Shiny 自带默认值是 5 MB，基本挡住所有真实的 feature table。
- **`FEATUREMAP_AUTH_ENABLED`** —— 登录开关。本地/课题组内部使用保持 `false`（默认）；公开公网版本设成 `true` 后，分析页面需要登录。设成 `true` 时，app 启动阶段会检查：`DBI`/`RSQLite`/`sodium` 是否已安装、`FEATUREMAP_SMTP_PASSWORD` 和 `FEATUREMAP_PUBLIC_URL` 是否已设置。缺任何一项都会拒绝启动，并在 `logs/app.log` 里列出缺了什么。测试时可以设 `FEATUREMAP_EMAIL_DRY_RUN=true` 跳过邮件相关的检查。这三个 R 包现在属于可选依赖（`Suggests`），`install_server.sh` 和 `run_app.sh update` 用 `dependencies = TRUE` 安装，会自动装上。
- **`FEATUREMAP_PUBLIC_MODE`** —— 公开部署开关，默认关闭。设成 `true` 后：自动开启登录（相当于同时设了 `FEATUREMAP_AUTH_ENABLED=true`）；LLM 和 NCBI 的 API key 必须由用户自己在页面上填写，服务器环境变量里的 `FEATUREMAP_LLM_API_KEY`、`FEATUREMAP_NCBI_API_KEY` 不会被使用，避免访客花你的额度；隐藏 Local provider，因为它的 Base URL 可以让访客让服务器去访问任意地址（包括内网机器）。本地和课题组服务器不要设。
- **`FEATUREMAP_TEMP_MAX_AGE_HOURS`** —— 每个会话在第 5 步生成的 HTML 放在会话自己的临时目录里，用户关闭页面时会自动删除；另外 app 每小时检查一次，删除超过这个时长（默认 24 小时）没有改动的目录，作为兜底。
- **`FEATUREMAP_BG_WORKERS`** —— 第 2-5 步的分析在后台 R 进程里运行，界面所在的主进程不再被占住：有人在跑分析时，其他人浏览、切换页面不受影响。这个变量是后台进程的数量，也就是同一时间最多跑几个分析。`run_app.sh` 里默认设为 `1`；设了 `FEATUREMAP_PUBLIC_MODE=true` 时默认也是 `1`；用户在自己电脑上本地安装时默认是 `0`，和以前完全一样。需要 `callr` 包和 shiny ≥ 1.8.1，缺任何一个时 app 会在日志里说明，并自动退回到旧的方式运行，不会起不来。多出来的任务按先来后到排队，点 Run 后弹出的运行窗口会告诉用户“前面还有几个任务”。每个后台进程连同它开的并行线程都要单独占内存，调大之前先确认内存够用。设成 `0` 则退回到旧的方式（在主进程里直接跑）。
- **`FEATUREMAP_MEMORY_LIMIT_GB`** —— 一个分析最多能用多少内存（后台 worker 加上它开出的所有并行进程），默认不限制。设置后每 2 秒检查一次，超过就停掉这个分析、提示用户，并换一个新的 worker，避免把整台服务器拖到内存耗尽。每一步跑完，日志里会记录 `peak memory`（内存峰值），可以据此调整。只在 Linux 上生效。公开服务器上建议同时设 `FEATUREMAP_MAX_THREADS=1`：第 2 步每多一个线程就要多 3 GB 以上内存。
- **`FEATUREMAP_MAX_THREADS`** —— 第 2-4 步 Threads 输入框的上限（服务器端也会强制限制）。每个线程是一个独立的 R 进程，所以一个分析的内存约等于（线程数 + 1）× 单个 R 进程的内存。默认：公开模式 8，否则 16。
- **`FEATUREMAP_LLM_TIMEOUT`** —— 单次 LLM 请求最多等多少秒，默认 300。会“思考”的模型（Gemini 2.5/3、SiliconFlow 上的推理模型）常常超过一分钟。超时、限流（429）和服务器错误（5xx）会自动重试；模型 ID 写错、key 无效这类错误不重试，直接报错。
- **`FEATUREMAP_AUTH_DB`** —— 账号 SQLite 数据库位置。公开部署时放在 `~/featuremap-deploy/` 这类持久目录，不要提交到 git。
- **`FEATUREMAP_PUBLIC_URL`** —— 公开访问地址，用来生成邮箱验证和重置密码链接。公网部署时一定要设成最终 URL。
- **`FEATUREMAP_SMTP_USERNAME` / `FEATUREMAP_SMTP_FROM`** —— 发件邮箱，默认 `featuremapofficial@gmail.com`。
- **`FEATUREMAP_SMTP_PASSWORD`** —— Gmail app password。不要提交到 git；在服务器环境变量或私有配置里设置。
- **`SILICONFLOW_BASE_URL`** —— 全组统一用某一个站点的 key 时取消注释，可以完全绕过端点自动探测。

### 配置 Gmail 发信

FeatureMAP 账号邮件默认使用 `featuremapofficial@gmail.com` 发送。第一次启用前：

1. 登录 `featuremapofficial@gmail.com` 的 Google Account。
2. 在 Security 里开启 2-Step Verification。
3. 打开 App passwords，创建一个用于 FeatureMAP SMTP 的 app password。
4. 在服务器私有环境里设置。推荐放到 `~/featuremap-deploy/env.sh`，这个文件在仓库外，不会提交到 git：

```bash
ssh yuting@10.96.129.143
nano ~/featuremap-deploy/env.sh
```

写入下面几行，把最后一行换成 Google 生成的 16 位 app password：

```bash
export FEATUREMAP_AUTH_ENABLED=true
export FEATUREMAP_PUBLIC_URL=https://你的公网地址
export FEATUREMAP_SMTP_USERNAME=featuremapofficial@gmail.com
export FEATUREMAP_SMTP_FROM=featuremapofficial@gmail.com
export FEATUREMAP_SMTP_PASSWORD='Google 生成的 app password'
```

`FEATUREMAP_PUBLIC_URL` 必须是用户能打开的公网地址，否则验证邮件和重置密码邮件里的链接会指错地方。

保存后重启应用，让新的环境变量进入 R 进程：

```bash
~/featuremap-deploy/src/deploy/run_app.sh restart
```

检查当前 R 进程能不能读到配置，但不要打印密码本身：

```bash
Rscript -e 'cat("user=", Sys.getenv("FEATUREMAP_SMTP_USERNAME"), "\npassword_chars=", nchar(Sys.getenv("FEATUREMAP_SMTP_PASSWORD")), "\ndry_run=", Sys.getenv("FEATUREMAP_EMAIL_DRY_RUN"), "\n")'
```

改法（改仓库文件，走正常发布流程）：

```bash
# Mac 上改 deploy/run_app.sh，然后
git add deploy/ && git commit -m "change port" && git push
# 服务器上
~/featuremap-deploy/src/deploy/run_app.sh update
```

**临时试一下不同配置**（不改文件，重启即失效）：

```bash
FM_PORT=3839 ./deploy/run_app.sh restart
```

---

## 6. 从零重建（换机器 / 环境坏了）

```bash
# 1. 先体检（只读，不改任何东西）
scp deploy/check_server.sh 用户名@新服务器:~/
ssh 用户名@新服务器
bash ~/check_server.sh

# 2. 配 GitHub 只读 deploy key
mkdir -p ~/.ssh && chmod 700 ~/.ssh
ssh-keygen -t ed25519 -f ~/.ssh/featuremap_deploy -N "" -C "featuremap-deploy@$(hostname)"
cat ~/.ssh/featuremap_deploy.pub
#    → 复制到 GitHub 仓库 Settings → Deploy keys → Add（不要勾 Allow write access）

cat >> ~/.ssh/config <<'EOF'

Host github.com
  HostName github.com
  User git
  IdentityFile ~/.ssh/featuremap_deploy
  IdentitiesOnly yes
EOF
chmod 600 ~/.ssh/config

ssh -T git@github.com
#    → 应回 "Hi tidymass/featuremap_shiny!"（报仓库名，不是用户名）

# 3. 安装
git clone git@github.com:tidymass/featuremap_shiny.git ~/featuremap-deploy/src
bash ~/featuremap-deploy/src/deploy/install_server.sh    # 可反复运行，已完成的会跳过

# 4. 启动
~/featuremap-deploy/src/deploy/run_app.sh start
```

**在 Mac 上把公钥直接抓进剪贴板**（避免终端换行截断导致 GitHub 报 "Key is invalid"）：

```bash
ssh yuting@10.96.129.143 'cat ~/.ssh/featuremap_deploy.pub' | pbcopy
```

---

## 7. 排障

### 组员打不开页面

```bash
# 服务是否在跑
~/featuremap-deploy/src/deploy/run_app.sh status

# 是否真的监听在 0.0.0.0（不是 127.0.0.1）
ss -ltnp | grep 3838

# 服务器本机能不能访问
curl -sI http://localhost:3838 | head -1
```

三项都正常但外部仍连不上 → 网络/防火墙问题，确认对方和服务器在同一内网。

### 启动失败

```bash
tail -50 ~/featuremap-deploy/logs/app.log
```

### 端口被占

```bash
ss -ltnp | grep 3838          # 看是谁占着
FM_PORT=3839 ./deploy/run_app.sh start
```

### 进程卡死、stop 停不掉

```bash
pkill -f run_app.R
rm -f ~/featuremap-deploy/app.pid
./deploy/run_app.sh start
```

### 怀疑 R 包出问题

```bash
R_ENVIRON_USER=~/featuremap-deploy/Renviron.install \
  Rscript -e '.libPaths(); library(featuremapshiny); sessionInfo()'
```

### 重装某个包

```bash
R_ENVIRON_USER=~/featuremap-deploy/Renviron.install \
R_PROFILE_USER=~/featuremap-deploy/Rprofile.install.R \
FM_CRAN=https://packagemanager.posit.co/cran/__linux__/noble/latest \
FM_LIB=~/featuremap-deploy/rlib \
  Rscript -e 'install.packages("包名", lib=.libPaths()[1])'
```

### 磁盘

```bash
df -h ~                          # home 在 /mnt/hdd1，之前已用 90%
du -sh ~/featuremap-deploy/*
```

---

## 8. 踩过的坑及原因

记下来，免得下次重新趟一遍。

**`Renviron.site` 把 `R_LIBS_USER` 硬指到 `/scratch`。** 站点文件里的硬赋值会覆盖你 export 的任何值，而 `/scratch` 是 sticky-bit 的临时区，可能被清理。所以安装和运行脚本都用 `R_ENVIRON_USER` 指向自己的 `Renviron.install`（R 先读站点文件、后读用户文件，用户文件胜出），**同时**在 `R_PROFILE_USER` 里设 `.libPaths()`。两者缺一不可：`remotes` 编译源码包时会另起 `R CMD INSTALL` 子进程，那个子进程解析 `LinkingTo` 头文件走的是 `R_LIBS`，不是父进程的 `.libPaths()`。

**没有 libglpk，又没有 sudo。** igraph 要链接它。`install_server.sh` 把 GLPK 编译进 `~/featuremap-deploy/local`，并用 `-Wl,-rpath,...` 把路径烧进 `igraph.so`，这样即使忘了设 `LD_LIBRARY_PATH` 也能加载。

**没有 pandoc。** `htmlwidgets::saveWidget(selfcontained = TRUE)` 需要它，缺了的话网络图导出会失败——而且是在跑完整个分析之后的最后一步才失败。已作为静态二进制装到 `~/featuremap-deploy/bin`。

**没有 tmux/screen。** 应用用 `nohup` + PID 文件后台运行。代价是服务器重启后不会自动拉起。要自启需要管理员执行 `loginctl enable-linger yuting`，然后配 `systemctl --user` 服务。

**`pcsf2featuremap` 的 `src/Makevars` 有 bug。** 它硬编码 `$(R_HOME)/library/Rcpp/include`，并引用了一个从未定义的 `$(BH_INCLUDE)`。后者展开成空，产生一个光秃秃的 `-I`，而 gcc 的 `-I` 作为独立 token 时会**吃掉下一个参数**——正好是 R 根据 `LinkingTo` 生成的那个正确的 Rcpp 路径。于是 `Rcpp.h` 明明就在命令行列出的目录里，却报找不到。已在上游删除该文件；`LinkingTo: Rcpp, BH` 本来就会提供正确路径。

**编译产物被提交进了 git**（`src/*.o`、`*.so`）。源码 tarball 会带上它们，`make` 可能跳过编译直接去链接一个为别的架构编译的目标文件。已 untrack 并加入 `.gitignore`。

---

## 9. 尚未处理

- **worker 数没有上限。** `workers`、`pcsf_workers`、`module_workers` 是自由填写的数字框。128 核余量很大，但没有任何机制阻止一个人占满。
- **没有认证。** 能连到实验室网络的人都能打开。组内可接受；对外开放前必须加。
- **git 历史里有 4 个 API key。** 在本仓库的 `fe47f2b` 和 `68278f6` 两个提交的 `R/40_annotate_module.R` 里（SiliconFlow / OpenAI / Gemini / NCBI）。**请尽快轮换。** 仓库虽然是私有的，但任何已存在的 clone 都带着它们。轮换之后再考虑用 `git filter-repo` 清理历史。
- **Docker 组权限。** 服务器装了 docker 但当前用户不在 `docker` 组。若以后想上 ShinyProxy（每用户独立容器 + 登录认证），需要管理员执行 `usermod -aG docker yuting`。
