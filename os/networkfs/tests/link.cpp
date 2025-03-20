#include <filesystem>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <gtest/gtest.h>

#include "lib/test.hpp"
#include "lib/util.hpp"

namespace fs = std::filesystem;

class LinkTest : public NfsTest {};

TEST_F(LinkTest, View) {
    ino_t ino = nfs.lookup(ROOT_INO, "file1").ino;
    nfs.link(ino, ROOT_INO, "file3");

    struct stat st;

    ASSERT_EQ(stat("file1", &st), 0);
    ASSERT_EQ(st.st_ino, ino);

    ASSERT_EQ(stat("file2", &st), 0);
    ASSERT_NE(st.st_ino, ino);

    ASSERT_EQ(stat("file3", &st), 0);
    ASSERT_EQ(st.st_ino, ino);
}

TEST_F(LinkTest, Create) {
    ASSERT_NO_THROW(fs::create_hard_link({"file2"}, {"file3"}));

    ino_t original_ino = nfs.lookup(ROOT_INO, "file2").ino;
    ino_t new_ino = nfs.lookup(ROOT_INO, "file3").ino;
    ASSERT_EQ(new_ino, original_ino);
}

TEST_F(LinkTest, Unlink) {
    ino_t ino = nfs.lookup(ROOT_INO, "file1").ino;
    nfs.link(ino, ROOT_INO, "file3");

    ASSERT_NO_THROW(fs::remove({"file3"}));
    ASSERT_FALSE(fs::exists({"file3"}));
    ASSERT_TRUE(fs::exists({"file1"}));

    ino_t final_ino = nfs.lookup(ROOT_INO, "file1").ino;
    ASSERT_EQ(final_ino, ino);

    uint64_t file3_status = nfs.lookup(ROOT_INO, "file3").status;
    ASSERT_EQ(file3_status, 4);
}

TEST_F(LinkTest, OtherDirectory) {
    nfs.clear();

    ino_t dir1 = nfs.create(ROOT_INO, "alpha", EntryType::DIRECTORY).ino;
    ino_t file = nfs.create(dir1, "file", EntryType::FILE).ino;

    ino_t dir2 = nfs.create(ROOT_INO, "beta", EntryType::DIRECTORY).ino;

    ASSERT_NO_THROW(fs::create_hard_link({"alpha/file"}, {"beta/file"}));

    lookup_response response = nfs.lookup(dir2, "file");
    ASSERT_EQ(response.status, 0);
    ASSERT_EQ(response.ino, file);
}
