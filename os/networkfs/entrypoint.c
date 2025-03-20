#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Ivanov Ivan");
MODULE_VERSION("0.01");

int networkfs_init(void) {
  printk(KERN_INFO "Hello, World!\n");
  return 0;
}

void networkfs_exit(void) { printk(KERN_INFO "Goodbye!\n"); }

module_init(networkfs_init);
module_exit(networkfs_exit);
