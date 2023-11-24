library(dplyr)
library(data.table)
library(ggplot2)
library(openxlsx)
library(scatterplot3d)
library(BMA)
library(psych)
library(GGally)
library(broom)
library(stringr)

str(Customer)
str(Sale)

summary(Customer)
summary(Sale)

Customer_nona <- Customer %>% na.omit()
Sale_nona <- Sale %>% na.omit()

# Customer không có giá trị NULL
# Sale có 160 giá trị NULL
##SalePrice có 135 giá trị NULL
##SalePrice_fix có 9 giá trị NULL (SalePrice_fix được chuyển thể từ $ sang VND bằng cách * tỉ giá 23)

Sale <- Sale %>% select(-SalePrice)

find_na <- which(apply(is.na(Sale),1,any))
sale_na <- Sale[find_na,]

# Thấy SaleDate có Minlà 1900-06-24 và 1900-06-25 => Replace 1900 thành 2020
Sale$SaleDate <- gsub("1900", "2020", Sale$SaleDate)
# SalePrice_fix chúng ta sẽ lấy giá trị trung bình để gán cho các giá trị NA
Sale$SalePrice_fix[is.na(Sale$SalePrice_fix)] <- median(Sale$SalePrice_fix,na.rm=T)

Sale_original <- Sale

Sale$Channel <- ifelse(Sale$PaymentType == "Thẻ" & is.na(Sale$Channel), "Online", Sale$Channel)
Sale$PaymentType <- ifelse(Sale$Channel == "Online" & is.na(Sale$PaymentType), "Thẻ", Sale$PaymentType)
find_na <- which(apply(is.na(Sale),1,any))
sale_na <- Sale[find_na,]
Sale %>% count(Reference) %>% filter(n == max(n))
Sale %>% count(Brand) %>% filter(n == max(n))
Sale %>% count(Shop) %>% filter(n == max(n))
Sale %>% count(SaleDate) %>% filter(n == max(n))

Sale$Brand <- ifelse(is.na(Sale$Brand), max(Sale$Brand, na.rm = TRUE), Sale$Brand)
Sale$Shop <- ifelse(is.na(Sale$Shop), max(Sale$Shop, na.rm = TRUE), Sale$Shop)
Sale$Reference <- ifelse(is.na(Sale$Reference), max(Sale$Reference, na.rm = TRUE), Sale$Reference)

# Kiểm tra tháng có lượt mua cao nhất

allDates <- data.table(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="day"))
setnames(allDates,"DATE")

Sale$SaleDate <- as.Date(Sale$SaleDate, format = "%Y-%m-%d")
Sale_by_date <- Sale %>% left_join(allDates, by = c("SaleDate"="DATE"))
hist(Sale$SaleDate, col = "blue", main = "Histogram of Date", border = "white", breaks = "months")
# Tháng 2 là tháng có tần suất mua hàng cao nhất => Lấy giá trị NA theo ngày mua cao nhất trong tháng 2
Sale$SaleDate <- ifelse(is.na(Sale$SaleDate), max(Sale$SaleDate, na.rm = TRUE), Sale$SaleDate)

# Kiểm tra lại Sale_nona và Sale_na sau khi xử lý dữ liệu NA
Sale_nona <- Sale %>% na.omit()
find_na <- which(apply(is.na(Sale),1,any))
sale_na <- Sale[find_na,]

#PHÂN TÍCH KHÁM PHÁ
#PHÂN TÍCH MÔ TẢ 
### SỬ DỤNG POWER BI

# Tính doanh thu theo shop và brand

Sale_nona_revenue_bradn <- Sale_nona %>% group_by(Brand) %>% summarize(revenue = sum(SalePrice_fix))
Sale_nona_revenue_shop <- Sale_nona %>% group_by(Shop) %>% summarize(revenue = sum(SalePrice_fix))

# Phân tích mô hình hồi quy của Customer
# Lựa chọn mô hình sử dụng phương pháp Bayes Model Avarage
MucDoHaiLong <- Customer_nona$MucDoHaiLong
YeuToThuongHieuSP <- Customer_nona$YeuToThuongHieuSP
YeuToCauHinh <- Customer_nona$YeuToCauHinh
YeuToTocDoXuLy <- Customer_nona$YeuToTocDoXuLy
YeuToGia <- Customer_nona$YeuToGia
YeuToThietKe <- Customer_nona$YeuToThietKe
YeuToUyTinNhaBanLe <- Customer_nona$YeuToUyTinNhaBanle
YeuToBaoHanh <- Customer$YeuToBaoHanh
YeuToKhuyenMai <- Customer$YeuToKhuyenMai
X=cbind(YeuToThuongHieuSP,YeuToCauHinh,
        YeuToTocDoXuLy,YeuToGia,YeuToThietKe,
        YeuToUyTinNhaBanLe,YeuToBaoHanh,YeuToKhuyenMai)
Y=MucDoHaiLong
search=bicreg(X,Y,strict=FALSE,OR=20)
summary(search)
imageplot.bma(search)

# Theo đồ thị phân tán thì ta thấy ở model 1, 2 các yếu tố: 
## Thương hiệu SP, Yếu tố Cấu hình, Yếu tố giá, Yếu tố Thiết kế, Uy Tín nhà bán lẻ sẽ ảnh hưởng đến mức độ hài lòng khách hàng
## Triển khai nghiên cứu mô hình hồi quy
# Phương trình nghiên cứu Y0 = B= + B1X +..+ BiX
# Kiểm định mô hình hồi quy
mlm <- lm(MucDoHaiLong~YeuToThuongHieuSP + YeuToCauHinh + YeuToGia + YeuToThietKe,data=Customer_nona)
summary(mlm)
anova(mlm)
augment(mlm)

# Cần kiểm tra xem nếu shop thường xuyên sale nhiều có ảnh hưởng đến uy tín nhà bán lẻ không?
summary(Customer_nona)
Customer_nona %>% group_by(KH_ID)
unique(Customer_nona$KH_ID[duplicated(Customer_nona$KH_ID)])

result <- strsplit(Sale_nona$Reference, ",")

# Tách giá trị trong ngoặc và giữ nguyên chúng
result <- lapply(result, function(x) {
  str_extract_all(x, "\\([^)]+\\)|[^,]+") %>% unlist
})

# Chuyển kết quả thành data frame
result_df <- as.data.frame(do.call(rbind, result))
