# gql-grammar

完整的GQL的antlr实现。

## 可视化

（需要java环境）

下载antlr
```
scripts/download_antlr4.sh
```
安装Ghostscript
```
/* centos */
yum install ghostscript
/* ubuntu */
apt-get install ghostscript
```
## 使用

根据*.g4生成java文件。*.java会被放到target目录下
```
scripts/antlr4 *.g4
```
编译
```
cd target && javac -cp "../lib/antlr-4.9.3-complete.jar" *.java && cd ..
```
利用antlr生成ps格式文件
```
mkdir output
scripts/grun GQL insertStatement -ps output/file.ps < example/insert/insert_node.gql
```
ps文件生成jpg
```
scripts/ps2jpg output/file.ps output/insert.jpg
```

