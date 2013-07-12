wget http://download.dnscrypt.org/libsodium/releases/libsodium-0.4.1.tar.gz
tar -zxvf libsodium-0.4.1.tar.gz
rm libsodium-0.4.1.tar.gz
cd libsodium-0.4.1
./configure
make
make check
sudo make install

