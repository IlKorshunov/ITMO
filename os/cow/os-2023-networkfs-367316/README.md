# Задание 4. Сетевая файловая система

Ядро Linux — [монолитное](https://en.wikipedia.org/wiki/Monolithic_kernel). Это означает, что все его части работают в общем адресном пространстве. Однако, это не означает, что для добавления какой-то возможности необходимо полностью перекомпилировать ядро. Новую функциональность можно добавить в виде _модуля ядра_. Такие модули можно легко загружать и выгружать по необходимости прямо во время работы системы.

С помощью модулей можно реализовать свои файловые системы, причём со стороны пользователя такая файловая система ничем не будет отличаться от [ext4](https://en.wikipedia.org/wiki/Ext4) или [NTFS](https://en.wikipedia.org/wiki/NTFS). В этом задании мы с Вами реализуем упрощённый аналог [NFS](https://en.wikipedia.org/wiki/Network_File_System): все файлы будут храниться на удалённом сервере, однако пользователь сможет пользоваться ими точно так же, как и файлами на собственном жёстком диске.

> Выполните задание в ветке `networkfs`.

## Системные требования

Мы рекомендуем при выполнении этого домашнего задания использовать отдельную виртуальную машину: любая ошибка может вывести всю систему из строя, и вы можете потерять ваши данные.

Для создания виртуальной машины вы можете воспользоваться любым гипервизором. Например, в Windows встроен Hyper-V, на Mac OS есть Parallels. Есть и кроссплатформенное решение — VirtualBox. Для него мы нашли хороший [туториал по установке гостевой ОС](https://ubuntu.com/tutorials/how-to-run-ubuntu-desktop-on-a-virtual-machine-using-virtualbox). Не забудьте разобраться, как в настройках вашего гипервизора включить поддержку аппаратной виртуализации. Например, в настройках VirtualBox она включается в разделе «System» настроек виртуальной машины, соответствующая опция называется «Hardware Virtualization».

Для удобной работы с ВМ рекомендуем расширение [Remote SSH](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-ssh) для Visual Studio Code или [JetBrains Gateway](https://www.jetbrains.com/help/clion/remote.html), встроенный в CLion.

Все инструкции написаны для ядра версии 6.2.0-31 и были проверены на Ubuntu 23.04 x64. На этой же версии ядра запускаются автоматические тесты. Возможно, при использовании других дистрибутивов и версий ядра, вы столкнётесь с различными ошибками и особенностями, с которыми вам придётся разобраться самостоятельно.

Чтобы установить нужное ядро на Ubuntu, выполните в терминале команду:

```bash
sudo apt install linux-image-6.2.0-31-generic
```

После этого перезагрузите вашу виртуальную машину. Убедитесь с помощью команды `uname -r`, что у вас ядро правильной версии.

## Сервер файловой системы

Все файлы и структура директорий хранятся на удалённом сервере. Сервер поддерживает HTTP API, документация к которому доступна [по ссылке](http://nerc.itmo.ru/teaching/os/networkfs/).

Для получения токенов и тестирования вы можете воспользоваться консольной утилитой [curl](https://curl.se).

Сервер поддерживает два типа ответов:
* Бинарные данные: набор байт (`char*`), который можно скастить в структуру, указанную в описании ответа. Учтите, что первое поле ответа (первые 8 байт) — код ошибки.
* [JSON-объект](https://www.json.org/json-en.html): человекочитаемый ответ. Для его получения необходимо передавать GET-параметр `json`.

Формат JSON предлагается использовать только для отладки, поскольку текущая реализация функции `networkfs_http_call` работает только с бинарным форматом. Однако, вы можете её доработать и реализовать собственный JSON-парсер.

> [!IMPORTANT]
>
> Для начала работы вам необходимо завести собственный бакет — пространство для хранения файлов, и получить токен для доступа к нему. Это делается следующим запросом:
> 
> ```sh
> $ curl https://nerc.itmo.ru/teaching/os/networkfs/v1/token/issue?json
> {"status":"SUCCESS","response":"8c6a65c8-5ca6-49d7-a33d-daec00267011"}
> ```

Строка `8c6a65c8-5ca6-49d7-a33d-daec00267011` и является токеном, который необходимо передавать во все последующие запросы. Количество токенов и размер файловой системы не ограничены, однако, мы будем вынуждены ограничить пользователей в случае злоупотребления данной возможностью.

Поскольку в ядре используются не совсем привычные функции для работы с сетью, мы реализовали для вас собственный HTTP-клиент в виде функции `networkfs_http_call` ([`http.c:120`](http.c#L120)):

```c
int64_t networkfs_http_call(
    const char *token,
    const char *method,
    char *response_buffer,
    size_t buffer_size,
    size_t arg_size,
    ...
);
```

* `const char *token` — ваш токен
* `const char *method` — название метода без неймспейса `fs` (`list`, `create`, …)
* `char *response_buffer` — буфер для сохранения ответа от сервера
* `size_t buffer_size` — размер буфера
* `size_t arg_size` — количество аргументов
* далее должны следовать 2 × `arg_size` аргументов типа `const char*` — пары `param1`, `value1`, `param2`, `value2`, … — параметры запроса

Функция возвращает 0, если запрос завершён успешно; положительное число — код ошибки из документации API, если сервер вернул ошибку; отрицательное число — код ошибки из [`http.h`](http.h#L6) или `errno-base.h` (`ENOMEM`, `ENOSPC`) в случае ошибки при выполнении запроса (отсутствие подключения, сбой в сети, некорректный ответ сервера, …).

## Знакомство с простым модулем

Давайте научимся компилировать и подключать тривиальный модуль. Для компиляции модулей ядра нам понадобятся утилиты для сборки и заголовочные файлы. Установить их можно так:

```sh
$ sudo apt-get install build-essential linux-headers-`uname -r` cmake
```

Мы уже подготовили основу для вашего будущего модуля в файле [`entrypoint.c`](entrypoint.c). Познакомьтесь с ним.

Ядру для работы с модулем достаточно двух функций — одна должна инициализировать модуль, а вторая — очищать результаты его работы. Они указываются с помощью макросов `module_init` и `module_exit`.

Важное отличие кода для ядра Linux от user-space-кода — в отсутствии в нём стандартной библиотеки `libc`. Например, в ней же находится функция `printf`. Мы можем печатать данные в системный лог с помощью функции [`printk`](https://www.kernel.org/doc/html/latest/core-api/printk-basics.html).

Сконфигурируем систему сборки:

```sh
$ mkdir build
$ cd build
$ cmake ..
```

Для удобства разработки и запуска мы настроили CMake. Вам не потребуется сильно его менять — если у вас появятся новые единицы трансляции, добавьте их в [`SOURCES`](CMakeLists.txt#L11). Пока что их две — `entrypoint` и `http`.

Чтобы собрать модуль, выполните

```
$ make
<...>
[100%] Built target networkfs
```

Если наш код скомпилировался успешно, в директории `build` появится файл `networkfs.ko` — это и есть наш модуль. Осталось загрузить его в ядро:

```sh
$ sudo insmod networkfs.ko
```

Однако, мы не увидели нашего сообщения. Оно печатается не в терминал, а в системный лог — его можно увидеть командой `dmesg`:

```sh
$ dmesg
<...>
[  123.456789] Hello, World!
```

Для выгрузки модуля нам понадобится утилита `rmmod`. Обратите внимание, что она принимает название модуля, а не файл с модулем:

```sh
$ sudo rmmod networkfs
$ dmesg
<...>
[  123.987654] Goodbye!
```

## Часть 1. Инициализация файловой системы

Наша точка входа — функция [`register_filesystem`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2605). Она сообщает ядру о появлении нового драйвера операционной системы.

Эта функция в качестве аргумента принимает указатель на структуру типа [`file_system_type`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L593). Она описывает доступные в файловой системе методы.

Несколько раз в нашем коде мы будем использовать один и тот же паттерн — создадим глобальную структуру, в которой описаны наши методы, и передадим в качестве аргумента функции. Например, так:

```c
struct file_system_type networkfs_fs_type = {
  .name = "networkfs"
};
```

Эта конструкция создаёт константную структуру с именем `networkfs_fs_type` типа `file_system_type` и инициализирует одно поле в ней — `name` — строкой `networkfs`.

Это имя мы передадим в аргумент `-t` команды [`mount`](https://linux.die.net/man/8/mount) для выбора нашей новой файловой системы.

Парная функция к `register_filesystem` — [`unregister_filesystem`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2606). Нетрудно догадаться, что она удаляет драйвер файловой системы.

Обе этих функции возвращают 0, если операция завершилась успешно.

> [!IMPORTANT]
>
> Добавьте в функции инициализации и выгрузки вашего модуля вызов соответственно функций `register_filesystem` и `unregister_filesystem`.
>
> При неуспешной регистрации пробросьте код ошибки в результат функции `networkfs_init`. При неуспешной выгрузке сообщите об ошибке в системный лог.

Далее мы начнём работать с несколькими структурами ядра. Давайте сразу познакомимся и с ними:
* [`inode`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L593) — описание метаданных файла: имя файла, расположение, тип файла (в нашем случае — регулярный файл или директория)
* [`dentry`](https://github.com/torvalds/linux/blob/v6.2/include/linux/dcache.h#L82) — описание директории: список `inode` внутри неё, информация о родительской директории, …
* [`super_block`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L1473) — описание всей файловой системы: информация о корневой директории, …

Пока что написанного кода недостаточно, чтобы файловая система заработала. Сначала при монтировании файловой системы нужно[^1] инициализировать контекст файловой системы — структуру типа [`fs_context`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs_context.h#L90). Это можно сделать с помощью метода `init_fs_context`.

```c
  int (*init_fs_context)(struct fs_context *);
```

Этот метод принимает вновь созданную структуру `fs_context`, и может добавить в неё некоторые поля, специфичные именно для нашей файловой системы. Нам понадобится заполнить всего одно поле — `ops` — и это ещё одна структура, типа [`fs_context_operations`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs_context.h#L115). 

Нам будет достаточно всего одного метода `.get_tree` — он отвечает за инициализацию суперблока файловой системы:

```c
struct fs_context_operations networkfs_context_ops = {
  .get_tree = networkfs_get_tree
};

int networkfs_init_fs_context(struct fs_context *fc) {
  fc->ops = &networkfs_context_ops;
  return 0;
}
```

Метод `get_tree` должен создать суперблок и записать его в контекст файловой системы. В ядре Linux уже реализовали функции для некоторых стандартных случаев — они называются `get_tree_...` в зависимости от того, каким образом будут храниться данные файловой системы. У нас нет физического раздела с данными — все файлы мы храним на сервере — поэтому нам нужна функция [`get_tree_nodev`](https://github.com/torvalds/linux/blob/v6.2/fs/super.c#L1153). Она принимает два аргумента: уже известный нам `struct fs_context*` и функцию `int fill_super(struct super_block *sb, struct fs_context *fc)`. Эта функция, в свою очередь, отвечает за инициализацию _суперблока_ нашей файловой системы — «корневую» структуру экземпляра файловой системы, которая содержит всю важную метаинформацию.

В этой функции нам понадобится создать нашу первую inode — она будет отвечать за корневую директорию. Поскольку inode у нас будет много, давайте напишем удобную функцию, которая будет это делать для нас. Начнём с тривиальной реализации:

```c
/**
 * @sb:     Суперблок файловой системы.
 * @parent: Родительская inode (NULL для корня ФС).
 * @mode:   Битовая маска из прав доступа и типа файла: https://github.com/torvalds/linux/blob/v6.2/include/uapi/linux/stat.h#L9.
 * @i_ino:  Уникальный идентификатор inode.
 */
struct inode *networkfs_get_inode(struct super_block *sb, const struct inode *parent, umode_t mode, int i_ino) {
  struct inode *inode;
  inode = new_inode(sb);

  if (inode != NULL) {
  inode->i_ino = i_ino;
    inode_init_owner(&init_user_ns, inode, parent, mode);
  }

  return inode;
}
```

Внутри мы просто вызываем функцию [`new_inode`](https://github.com/torvalds/linux/blob/v6.2/fs/inode.c#L1040), которая создаёт пустую ноду, указываем её идентификатор (`i_ino`) и задаём права доступа функцией [`inode_init_owner`](https://github.com/torvalds/linux/blob/v6.2/fs/inode.c#L2293).

Теперь можем вернуться к `fill_super` и `get_tree`:

```c
int networkfs_fill_super(struct super_block *sb, struct fs_context *fc) {
  // Создаём корневую inode
  struct inode *inode = networkfs_get_inode(sb, NULL, S_IFDIR, 1000);
  // Создаём корень файловой системы
  sb->s_root = d_make_root(inode);

  if (sb->s_root == NULL) {
    return -ENOMEM;
  }

  return 0;
}

int networkfs_get_tree(struct fs_context *fc) {
  int ret = get_tree_nodev(fc, networkfs_fill_super);

  if (ret != 0) {
    printk(KERN_ERR "networkfs: unable to mount: error code %d", ret);
  }

  return ret;
}
```

> На сервере номер корневой ноды всегда будет равен 1000.

Наконец, для успешной работы нам нужно не только уметь монтировать файловую систему, но и отмонтировать её. Для этого в `file_system_type` предусмотрен метод `kill_sb`. Пока нам не нужно ничего дополнительно освобождать при отмонтировании ФС, поэтому давайте создадим пустую функцию и добавим её в `.kill_sb` нашей файловой системы:

```c
void networkfs_kill_sb(struct super_block *sb) {
    printk(KERN_INFO "networkfs: superblock is destroyed");
}
```

Последний штрих в этой части — возможность работы с различными «бакетами» на сервере. Хардкодить токен, полученный ранее, в самом коде модуля — плохая идея. Лучше дать возможность пользователю самому предоставлять токен при монтировании файловой системы. Специально для этого в команда `mount` принимает аргумент `source`. Мы его можем получить в контексте ФС в поле `fc->source`.

В суперблоке для хранения произвольных данных драйвера файловой системы есть поле `s_fs_info` — в него можно записать указатель на ваши данные.

> [!IMPORTANT]
>
> Добавьте поддержку токенов в вашу файловую систему. Вам нужно копировать токены в ваш суперблок при его инициализации и освобождать память при его уничтожении.
>
> Выведите токен в `kill_sb` перед его освобождением, чтобы убедиться, что вы всё сделали правильно.

Самое время попробовать собрать модуль и примонтировать нашу систему:

```sh
$ sudo mkdir /mnt/networkfs  # Директория для монтирования должна быть пуста
$ make
$ sudo insmod networkfs.ko
$ sudo mount -t networkfs 8c6a65c8-5ca6-49d7-a33d-daec00267011 /mnt/networkfs
```

Если вы всё правильно сделали, ошибок возникнуть не должно. Тем не менее, перейти в директорию `/mnt/networkfs` не выйдет — ведь мы ещё не реализовали никаких функций для навигации по ФС.

Теперь отмонтируем файловую систему — это тоже должно получиться:

```sh
$ sudo umount /mnt/networkfs
$ sudo dmesg  # Поищите токен тут
```

## Часть 2. Вывод файлов и директорий

> В базовой версии задания все имена файлов и директорий состоят только из латинских букв, цифр, символов подчёркивания, точек и дефисов.

В прошлой части мы закончили на том, что не смогли перейти в директорию:

```sh
$ sudo mount -t networkfs 8c6a65c8-5ca6-49d7-a33d-daec00267011 /mnt/networkfs
$ cd /mnt/networkfs
bash: cd: /mnt/networkfs: Not a directory
```

Это происходит, потому что ядро не смогло найти нужную `inode` — мы её создали, но нигде её не возвращаем. В поле `i_op` каждой `inode` необходимо добавить структуру  [`inode_operations`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2137), которая задаёт возможный набор действий с `inode`.

Первый метод, который нам нужно определить — `lookup`. Функция должна по `inode` директории и `dentry` элемента в этой директории найти соответствующий `inode` файла.

Пока никаких файлов у нас нет, так что начнём с тривиальной реализации:

```c
struct dentry* networkfs_lookup(struct inode *parent, struct dentry *child, unsigned int flag) {
  return NULL;
}

struct inode_operations networkfs_inode_ops =
{
  .lookup = networkfs_lookup,
};
```

> [!IMPORTANT]
>
> Добавьте эту структуру в каждую `inode` при создании.

Если мы заново попробуем повторить переход в директорию, у нас ничего не получится — но уже по другой причине:

```c
$ cd /mnt/networkfs
-bash: cd: /mnt/networkfs: Permission denied
```

Решите эту проблему. Пока сложной системы прав у нас не будет — у всех объектов в файловой системе могут быть права `777`. В итоге должно получиться что-то такое:

```sh
$ ls -l /mnt/
total 0
drwxrwxrwx 1 root root 0 Nov  1 13:37 networkfs
```

Теперь мы можем перейти в `/mnt/networkfs`, но не можем вывести содержимое директории. Действительно: мы нигде его не определили.

Следующая структура, которую нам нужно реализовать, имеет тип [`file_operations`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2091). Она отвечает за возможные действия с конкретной `inode`. Эта структура находится в поле `i_fop` структуры `inode`.

В неё мы добавим метод `iterate` — эта функция позволяет получить список файлов в директории.

Функция `iterate` должна вызывать `dir_emit` для каждого файла в директории, а также для себя и для родительской директории — `.` и `..` соответственно. Она должна начинать вывод с позиции, записанной `ctx->pos`, и инкрементировать его после каждой новой записи. В качестве возвращаемого значения нужно вернуть число выведенных файлов.

Начнём с базового примера функции, которая возвращает один файл в директории `test.txt` с номером `inode` 101:

```c
int networkfs_iterate(struct file *filp, struct dir_context *ctx) {
  struct dentry *dentry = filp->f_path.dentry;
  struct inode *inode = dentry->d_inode;

  loff_t record_counter = 0;

  while (true) {
    switch (ctx->pos) {
      case 0:
        dir_emit(ctx, ".", 1, inode->i_ino, DT_DIR);
        break;

      case 1:
        struct inode *parent_inode = dentry->d_parent->d_inode;
        dir_emit(ctx, "..", 2, parent_inode->i_ino, DT_DIR);
        break;

      case 2:
        dir_emit(ctx, "test.txt", strlen("test.txt"), 1001, DT_REG);
        break;
      
      default:
        return record_counter;
    }

    ++record_counter;
    ++ctx->pos;
  }
}

struct file_operations networkfs_dir_ops =
{
  .iterate = networkfs_iterate,
};
```

Не забудьте записать полученную структуру в `i_fop`. Попробуем снова получить список файлов:

```sh
$ ls /mnt/networkfs
ls: cannot access '/mnt/networkfs/test.txt': No such file or directory
test.txt
```

Ошибка возникла из-за того, что мы не реализовали честный `lookup`. Мы это исправим чуть позже.

Для завершения этой части осталось реализовать `iterate` для корневой директории с запросом к серверу.

## Часть 3. Навигация по директориям

Давайте исправим ошибку, с которой мы встретились ранее — научимся «искать» наши файлы. В функцию `lookup` передаются `inode` родительской директории и `dentry` для того файла, который мы хотим найти.

Наша задача — найти (а точнее, создать) `inode` нашего файла и «добавить» его в `dentry`. Для этого существует функция [`d_add`](https://github.com/torvalds/linux/blob/v6.2/fs/dcache.c#L2811).

```c
struct dentry *networkfs_lookup(struct inode *parent, struct dentry *child, unsigned int flag) {
  const char *name = child->d_name.name;

  if (parent->i_ino == 1000 && !strcmp(name, "test.txt")) {
    struct inode* inode = networkfs_get_inode(parent->i_sb, NULL, S_IFREG, 1001);
    d_add(child, inode);
  }

  return NULL;
}
```

Убедитесь, что теперь ошибки при `ls` не возникает.

Реализуйте навигацию по файлам и директориям, используя данные с сервера.

## Часть 4. Создание и удаление файлов

Теперь научимся создавать и удалять файлы. Добавим ещё два метода в `inode_operations` — `create` и `unlink`.

Для того, чтобы продемонстрировать работу функций, заведём глобальную переменную `bool has_test_txt = true;` — с её помощью мы будем эмулировать удаление файла.

Подправим `networkfs_iterate` и `networkfs_lookup`:

```c
      case 2:
        if (has_test_txt) {
          dir_emit(ctx, "test.txt", strlen("test.txt"), 1001, DT_REG);
          break;
        } else {
          // Файлов больше нет
          return record_counter;
        }

<...>

  if (parent->i_ino == 1000 && !strcmp(name, "test.txt") && has_test_txt) {
```

Начнём с `unlink`: функция удаляет файл и возвращает 0:

```c
int networkfs_unlink(struct inode *parent, struct dentry *child) {
  const char *name = child->d_name.name;

  if (parent->i_ino == 1000 && !strcmp(name, "test.txt")) {
    has_test_txt = false;
  }
 
  return 0;
}
```

`create` чуть сложнее — он должен, как и `lookup`, добавлять новую `inode` с помощью функции `d_add` при успешном создании файла. Простой пример:

```c
int networkfs_create(struct user_namespace *user_ns, struct inode *parent, struct dentry *child, umode_t mode, bool b) {
  const char *name = child->d_name.name;
  if (parent->i_ino == 100 && !strcmp(name, "test.txt")) {
    has_test_txt = true;
    inode = networkfs_get_inode(parent->i_sb, NULL, S_IFREG, 1001);
    d_add(child, inode);
  }

  return 0;
}
```

Чтобы проверить, как создаются файлы, воспользуемся утилитой `touch`:

```sh
$ ls /mnt/networkfs
test.txt
$ rm /mnt/networkfs/test.txt
$ ls /mnt/networkfs
$ touch /mnt/networkfs/test.txt
$ ls /mnt/networkfs
test.txt
$
```

> Обратите внимание, что утилита `touch` проверяет существование файла: для этого вызывается функция `lookup`.

Вам нужно вместо использования глобального флага делать соответствующие запросы на сервер.

## Часть 5. Создание и удаление директорий

Следующая (и последняя из обязательных) часть нашего задания — создание и удаление директорий. Добавьте в `inode_operations` ещё два метода — `mkdir` и `rmdir`. Их сигнатуры можно найти [тут](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2137).

Если вы всё сделали правильно, теперь вы сможете запустить тесты. Для начала соберите их. Это нужно сделать один раз за всё время разработки (и, может, ещё один, если мы решим добавить новые тесты):

```sh
../build$ make networkfs_test
```

Запустите тесты и проверьте, что ваше решение работает:

```sh
$ sudo ctest --preset base --output-on-failure
<...>
100% tests passed, 0 tests failed out of 17
```

> Вы можете запустить один тест с помощью флага `-R`: 
> 
> ```sh
> $ sudo ctest -R BaseTest.ListEmpty --output-on-failure
> ```

Запуск тестов перед началом автоматически собирает новую версию модуля ядра. Однако, если у вас к моменту запуска тестов уже подключен модуль, то он не будет заменён на вновь собранный.

## Часть 6*. Произвольные имена файлов (1 балл)

> [!IMPORTANT]
>
> В этот раз вы можете выполнить любое количество бонусных заданий — баллы суммируются. 
> Вместе с тем, все тесты всё ещё нужно сдавать вместе с основным заданием.

Реализуйте возможность создания файлов и директорий, состоящих из любых печатных символов, кроме символа `/`. Пример команды, которая можно будет исполнить:

```sh
$ touch '!@#$%^&*()-+ '
$ ls
'!@#$%^&*()-+ '
```

Если вы всё сделали правильно, пройдёт тестовый набор `encoding`:

```sh
$ sudo ctest --preset encoding --output-on-failure
<...>
100% tests passed, 0 tests failed out of 5
```

## Часть 7*. Чтение и запись в файлы (2 балла)

Реализуйте чтение из файлов и запись в файлы. Для этого вам понадобится структура `file_operations` не только для директорий, но и для обычных файлов.

В этой структуре вам понадобится реализовать шесть методов:

* [`int open(struct inode *inode, struct file *filp)`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2107) — вызывается при открытии файла

   - `inode` — inode открываемого файла
   - `filp` — файловый дескриптор

   Используйте этот метод, чтобы получить текущее содержимое файла с сервера и записать его в буфер. Для хранения указателя на буфер отлично подходит поле [`file->private_data`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L969). Не забудьте сохранить размер файла в `inode->i_size`.

   Верните 0 в случае успеха или соответствующий код ошибки.

   В поле `filp->f_flags` хранятся флаги доступа к файлу. Поддержите один — `O_APPEND`: если он установлен, необходимо установить курсор в конец файла. Это можно сделать с помощью функции [`generic_file_llseek`](https://github.com/torvalds/linux/blob/v6.2/fs/read_write.c#L144).

* [`ssize_t read(struct file *filp, char *buffer, size_t len, loff_t *offset)`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2094) — вызывается для чтения некоторого фрагмента файла

   - `filp` — файловый дескриптор
   - `buffer` — буфер для записи результата
   - `len` — максимальное количество байт для чтения
   - `offset` — позиция, с которой нужно начинать чтение

  Верните количество прочитанных байт в случае успеха или отрицательный код ошибки.

  Не забудьте увеличить значение `offset` на количество прочитанных байт.

  > Обратите внимание, что просто так обратиться в `buffer` нельзя, поскольку он находится в user-space. Используйте [специальные функции](https://www.kernel.org/doc/htmldocs/kernel-hacking/routines-copy.html) для чтения и записи.

* [`ssize_t write(struct file *filp, const char *buffer, size_t len, loff_t *offset)`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2095) — вызывается для записи некоторого фрагмента файла

   - `filp` — файловый дескриптор
   - `buffer` — буфер для данных для записи
   - `len` — размер буфера
   - `offset` — позиция, с которой нужно начинать запись

  Если `*offset + len > 512`, запишите только данные до 512 байт. Если `*offset == 512`, верните `-EDQUOT` и ничего не записывайте.
   
* [`int flush(struct file* filp, fl_owner_t id)`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2108) — вызывается для сохранения файла при закрытии  
  [`int fsync(struct file* filp, loff_t begin, loff_t end, int datasync)`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2110) — вызывается для сохранения открытого файла

   - `filp` — файловый дескриптор
   - `id` — владелец блокировки, игнорируйте этот аргумент
   - `begin`, `end` — начало и конец записываемого фрагмента — игнорируйте эти аргументы и всегда пишите весь файл
   - `datasync` — флаг, показывающий, что синхронизировать метаданные не нужно — игнорируйте этот аргумент

   Отправьте в этот момент данные файла на сервер.

* [`int release(struct inode* inode, struct file* filp)`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2109) — вызывается при закрытии всех экземпляров файла

   - `inode` — inode файла
   - `filp` — файловый дескриптор

   Освободите всю память, которую вы аллоцировали.

Также вам понадобится метод `.llseek`. Он отвечает за перемещение текущего оффсета в файле. Реализовывать его самому не нужно, воспользуйтесь уже знакомой вам `generic_file_llseek`.

Попробуйте примонтировать файловую систему. У вас возникнет проблема:

```sh
$ cat file1
hello world from file1
$ echo "other" > file1
other
world from file1
```

Чтобы её решить, вам понадобится добавить метод [`setattr`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L2158) в `inode_operations`.

```c
int networkfs_setattr(struct user_namespace *user_ns, struct dentry *entry, struct iattr *attr);
```

Этот метод в начале всегда должен вызывать функцию [`setattr_prepare`](https://github.com/torvalds/linux/blob/master/fs/attr.c#L165). Она проверяет корректность вызова. Не забудьте установить в суперблоке атрибут `s_maxbytes`, чтобы избежать слишком длинных файлов.

Размер файла нужно изменить в случае, если битмаска [`iattr->ia_valid`](https://github.com/torvalds/linux/blob/master/include/linux/fs.h#L227) содержит флаг [`ATTR_OPEN`](https://github.com/torvalds/linux/blob/v6.2/include/linux/fs.h#L205). Ожидаемый новый размер файла хранится в `iattr->ia_size`.

В результате вы сможете сделать вот так:

```sh
$ cat file1
hello world from file1
$ cat file2
file2 content here
$ echo "hello" > file1
$ cat file1
hello
$ echo "world" >> file1
$ cat file1
hello
world
$ vim -n file1
<откроется Vim, нажмите :q! для выхода>
```

> Обратите внимание, что файл должен уметь содержать любые ASCII-символы с кодами от 0 до 127 включительно.

Если вы всё сделали правильно, пройдёт тестовый набор `file`:

```sh
$ sudo ctest --preset file --output-on-failure
<...>
100% tests passed, 0 tests failed out of 16
```

> Если вы не делали предыдущий бонус, у вас не пройдёт тест `WriteSpecial`. Это допустимо.

## Часть 8*. Жёсткие ссылки (+1 балл)

Вам необходимо поддержать возможность сослаться из разных мест файловой системы на одну и ту же `inode`.

> Обратите внимание: сервер поддерживает жёсткие ссылки только для регулярных файлов, но не для директорий.

Для этого добавьте поле `link` в структуру `inode_operations`. Сигнатура соответствующей функции выглядит так:

```c
int networkfs_link(struct dentry *target, struct inode *parent, struct dentry *child);
```

После реализации функции вы сможете выполнить следующие команды:

```sh
$ ln file1 file3
$ cat file1
hello world from file1
$ cat file3
hello world from file1
$ echo "test" > file1
$ rm file1
$ cat file3
test
$
```

> Если вы не делали предыдущий бонус, вы всегда можете проверить работу ссылок через команду `stat`: в свежесозданной ссылке будет тот же номер inode, что и в оригинальном файле.

Если вы всё сделали правильно, пройдёт тестовый набор `link`:

```sh
$ sudo ctest --preset link --output-on-failure
<...>
100% tests passed, 0 tests failed out of 5
```

## Часть 9*. Rust (+2 балла)

Напишите ваш модуль не на C, а на Rust.

Вам потребуется также самостоятельно портировать HTTP-клиент и поправить скрипты сборки, чтобы использовать `rustc`.

Все необходимые инструменты для разработки на Rust уже установлены в CI — вам необходимо лишь поправить команду сборки: либо научить CMake собирать тесты по команде `make networkfs`, либо в [workflow](.github/workflows/networkfs.yml#L57) добавить команды для сборки вашего модуля.

В результате выполнения этого шага сборки в директории `build` должны находиться два файла: `networkfs_test` (собранные тесты) и `networkfs.ko`.

Полезные ссылки, с которых можно начать:
- https://wusyong.github.io/posts/rust-kernel-module-00/
- https://github.com/Rust-for-Linux/rust-out-of-tree-module

[^1]: Существует альтернативный путь — определить метод `mount` в `file_system_type`, который должен возвращать корневую директорию файловой системы, однако он считается устаревшим.
