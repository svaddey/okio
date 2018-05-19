package com.braviithi.okio.service;

import java.io.File;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.GetObjectRequest;
import com.amazonaws.services.s3.model.PutObjectRequest;

@Service
public class S3Service implements IS3Service {
	
	@Autowired
	private AmazonS3 s3client;
	
	@Value("${okio.s3.bucket}")
	private String bucketName;
	
	@Value("${okio.md5.salt}")
	private String md5Salt;
	
	private String getAWSKey(String input, String fileExtension) {
		if (StringUtils.hasText(input)) {
			StringBuilder sb = new StringBuilder();
			sb.append(input.charAt(0)).append("/").append(input.charAt(1))
				.append("/").append(input.charAt(2)).append("/")
				.append(input).append(".").append(fileExtension);
			
			return sb.toString();
		}
		
		return null;
	}

	@Override
	public void downloadFile(String keyName) {
        s3client.getObject(new GetObjectRequest(bucketName, keyName));
	}
	
	@Override
	public String uploadFile(String keyName, File file) {
		String fileExtension = FilenameUtils.getExtension(keyName);
		if (StringUtils.hasText(keyName) && StringUtils.hasText(md5Salt)) {
			String md5Hash = DigestUtils.md5Hex(keyName + md5Salt);
			String key = getAWSKey(md5Hash, fileExtension);
	        s3client.putObject(new PutObjectRequest(bucketName, key, file));
	        
	        return key;
		}
		
		return null;
	}
	
	@Override
	public void modifyACL(String keyName) {
        s3client.setObjectAcl(bucketName, keyName, CannedAccessControlList.PublicRead);
	}
}
