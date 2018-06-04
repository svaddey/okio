package com.braviithi.okio.service;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import com.braviithi.okio.dto.ReportDto;

@Service
public class ReportService implements IReportService {
	
	@Autowired
	private IS3Service s3Service;
	
	@Autowired
	private IRService rService;
	
	@Value("${okio.s3.bucket}")
	private String bucketName;
	
	@Value("${okio.s3.url}")
	private String s3Url;
	
	private File convert(MultipartFile file) {
		File convFile = new File(file.getOriginalFilename());
		try {
			convFile.createNewFile(); 
			FileOutputStream fos = new FileOutputStream(convFile); 
			fos.write(file.getBytes());
			fos.close();
		} catch (FileNotFoundException e) {
			return null;
		} catch (IOException e) {
			return null;
		} 
		return convFile;
	}
	
	private String getObject(String key, String type) {
		if (StringUtils.hasText(key) && StringUtils.hasText(type)) {
			String[] strs = key.split("\\.");
			StringBuilder sb = new StringBuilder();
			sb.append(strs[0]).append("_").append(type).append(".").append(strs[1]);
			
			return sb.toString();
		}
		return null;
	}
	
	private String getUrl(String key, String type) {
		if (StringUtils.hasText(key) && StringUtils.hasText(type)) {
			String[] strs = key.split("\\.");
			StringBuilder sb = new StringBuilder(s3Url);
			sb.append("/").append(bucketName).append("/").append(strs[0]).append("_").append(type).append(".").append(strs[1]);
			
			return sb.toString();
		}
		return null;
	}
	
	@Override
	public void uploadFile(ReportDto reportDto) {
		MultipartFile multipartFile = reportDto.getFile();
		File file = convert(multipartFile);
		String awsKey = s3Service.uploadFile(multipartFile.getOriginalFilename(), file);
//		rService.processImage(awsKey, reportDto);
		s3Service.modifyACL(getObject(awsKey, "revised"));
		s3Service.modifyACL(getObject(awsKey, "report"));
		reportDto.setImageUrl(getUrl(awsKey, "revised"));
		reportDto.setReportUrl(getUrl(awsKey, "report"));
	}

}
